private
convertRet: assoc forSeq: seq 
	| push ret |
	push := assoc key.
	ret := assoc value.
	seq replaceNode: push withNode: (push quickRetNode sourceNode: ret sourceNode).
	seq remove: ret.