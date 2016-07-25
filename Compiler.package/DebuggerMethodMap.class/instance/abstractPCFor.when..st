source mapping
abstractPCFor: concretePC when: contextIsActive
	"If the context is the actve context (is at the hot end of the stack)
	 then its pc is the current pc.  But if the context isn't, because it is
	suspended sending a message, then its current pc is the previous pc"
	^self  abstractPCForConcretePC:
		(contextIsActive 
			ifTrue: [concretePC]
			ifFalse: [(self method pcPreviousTo: concretePC) ifNil: [concretePC]]) method: self method