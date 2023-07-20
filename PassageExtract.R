packWords<-function(glove.model, seed.term.list, num=100){
  all.related.terms<-lapply(seed.term.list, function(x) getTopTerms(glove.model, x, num))
  all.related.terms<-unlist(all.related.terms)
  summarized.related.terms<-tapply(all.related.terms, names(all.related.terms), "sum")
  summarized.related.terms<-sort(summarized.related.terms, decreasing=T)
  return(summarized.related.terms)
}


#################################
#Functions to find domestic space passages in parsed files

findRoom<-function(parsed.text, room.term){
  room.match<-which(parsed.text$lemma==room.term)
  if(length(room.match>0)){
    sentence.number<-parsed.text$sentence_id[room.match]
    room.id<-rep(room.term, length(room.match))
    offset<-parsed.text$token_id[room.match]
    if(length(room.match)==1){
      term.table<-c(room.id, sentence.number, offset)
      names(term.table)<-c("RoomID", "SentenceNumber", "Offset")
    } else {
      term.table<-data.frame(room.id, sentence.number, offset)
      colnames(term.table)<-c("RoomID", "SentenceNumber", "Offset")
    }
    return(term.table)
  }
}

fetchSentences<-function(parse.table, sentence.vector){
  parse.collapse<-parse.table[which(parse.table$sentence_id %in% sentence.vector),]
  parse.collapse<-parse.collapse[-which(duplicated(parse.collapse$sentence_id)),]
  text.segment<-paste(parse.collapse$sentence, collapse=" ")
  return(text.segment)
}

findAllRooms<-function(parsed.text, room.term.list, bigram.terms=c("living", "dining", "sitting", "drawing", "dressing"), sentences.before=1, sentences.after=5){
  term.match<-which(room.term.list %in% parsed.text$lemma)
  if(length(term.match)>0){
    room.term.list<-room.term.list[term.match]
    room.match.tables<-lapply(room.term.list, function(x) findRoom(parsed.text, x))
    room.match.table<-do.call("rbind", room.match.tables)
    room.match.table<-as.data.frame(room.match.table)
    room.match.table[,2]<-as.numeric(room.match.table[,2])
    room.match.table[,3]<-as.numeric(room.match.table[,3])
    bigram.check<-which(room.match.table$RoomID %in% bigram.terms)
    if(length(bigram.check>0)){
      bigram.sentences<-room.match.table$SentenceNumber[bigram.check]
      room.index<-which(parsed.text$lemma=="room")
      if(length(room.index)>0){
        room.sentences<-parsed.text$sentence_id[room.index]
        bigrams.without.room<-which(!bigram.sentences %in% room.sentences)
        if(length(bigrams.without.room)>0){
          bad.bigrams<-bigram.check[bigrams.without.room]
          room.match.table<-room.match.table[-bad.bigrams,]
        }
      } else {
        room.match.table<-room.match.table[-bigram.check,]
      }
    }
    sentence.start.index<-room.match.table$SentenceNumber-sentences.before
    bad.sentences<-which(sentence.start.index<1)
    if(length(bad.sentences)>0){
      sentence.start.index[bad.sentences]<-1
    }
    max.sentences<-max(parsed.text$sentence_id)
    sentence.end.index<-room.match.table$SentenceNumber+sentences.after
    bad.sentences<-which(sentence.end.index>max.sentences)
    if(length(bad.sentences)>0){
      sentence.end.index[bad.sentences]<-max.sentences
    }
    all.sentence.seq<-mapply(function(x,y) seq(x,y,by=1), sentence.start.index, sentence.end.index, SIMPLIFY=F)
    all.sentence.blocks<-lapply(all.sentence.seq, function(x) paste(fetchSentences(parsed.text, x), collapse=" "))
    all.sentence.blocks<-unlist(all.sentence.blocks)
    room.match.table$TextSegment<-all.sentence.blocks
    return(room.match.table)
  }
}

findRoomsText<-function(parsed.row, parse.folder, room.terms.list){
  text.filename<-gsub("txt", "csv", parsed.row[1])
  text.filename<-paste(parse.folder, text.filename, sep="/")
  parsed.text<-read.csv(text.filename)
  text.title<-parsed.row[2]
  text.author<-parsed.row[3]
  text.date<-parsed.row[4]
  text.table<-findAllRooms(parsed.text, room.terms.list)
  if(length(text.table)>0){
    text.table$Filename<-rep(text.filename, nrow(text.table))
    text.table$title<-rep(text.title, nrow(text.table))
    text.table$author<-rep(text.author, nrow(text.table))
    text.table$date<-rep(text.date, nrow(text.table))
    return(text.table)
  }
}

findRoomsCorpus<-function(metadata.table, parse.folder, room.terms.list){
  all.text.rooms<-apply(metadata.table, 1, function(x) findRoomsText(x, parse.folder, room.terms.list))
  print("Creating Final Table")
  final.table<-do.call("rbind", all.text.rooms)
  return(final.table)
}