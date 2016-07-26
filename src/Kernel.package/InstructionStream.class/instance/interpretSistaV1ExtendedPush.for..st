decoding
interpretSistaV1ExtendedPush: extB for: client 
	"Implement the extended push for non-zero extensions."
	"*	82			01010010			Push thisContext, (then Extend B = 1 => push
	thisProcess) "
	extB = 0
		ifTrue: [^ client pushActiveContext].
	extB = 1
		ifTrue: [^ client pushActiveProcess].
	self error: 'undefined extended push'
