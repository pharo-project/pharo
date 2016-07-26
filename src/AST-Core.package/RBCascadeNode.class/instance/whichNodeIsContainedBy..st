querying
whichNodeIsContainedBy: anInterval 
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	messages 
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages 
				collect: [:each | each whichNodeIsContainedBy: anInterval]) 
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]