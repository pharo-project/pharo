private
convertStorePop: assoc forSeq: seq 
	| store pop |
	store := assoc key.
	pop := assoc value.
	seq replaceNode: store withNode: (store popIntoNode sourceNode: store sourceNode).
	seq remove: pop.