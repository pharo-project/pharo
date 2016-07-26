building
pointTailTo: anRxmLink
	"Propagate this message along the chain of links.
	Point `next' reference of the last link to <anRxmLink>.
	If the chain is already terminated, blow up."

	next == nil
		ifTrue: [next := anRxmLink]
		ifFalse: [next pointTailTo: anRxmLink]