Remember that `FALSE` means this is a **non-domestic space**: a passage that we want to train the model to see as _definitely not domestic space,_ not something indeterminate that simply doesn't seem very domestic.

The operative question for tagging is: _is most of this passage in a domestic space_? For our purposes, a domestic space is primarily a home (especially a house) that is someone's long-term residence.

A decision tree for tagging:
1. Is this in a space? If no, then `FALSE`
2. Can the space be identified (e.g., pure dialogue)? If no, then `FALSE`
3. Is this in one or multiple spaces? If only in one space or one space is a clear majority, continue; if not, then `NA`
4. Is the space indoors? If no, then `FALSE`
    - If in doubt: if it is on dirt, it is probably outdoors, otherwise, probably not. Gardens are outdoors, verandas and patios are not. 
5. Is the space accessible to the general public? If yes, then `FALSE`
    - Large buildings such as castles or palaces may have a publicly accessible component (non-domestic) and a private, domestic one
6. Is this space a non-domestic space where people live: a prison, monastery, abbey, convent, ship, university, school, barracks, religious house, etc.? If yes, then `FALSE`
7. Is it a cottage, hotel, cabin, campsite, chambers, etc. occupied on a short-term basis? If yes, then `FALSE`