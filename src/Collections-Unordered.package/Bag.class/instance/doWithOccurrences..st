enumerating
doWithOccurrences: aTwoArgBlock 
    "Iterate over the receiver and apply a two argument block on the element and its occurrences."
        
    contents associationsDo: [:assoc | aTwoArgBlock value: assoc key value: assoc value ]