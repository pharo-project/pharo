querying
bestNodeFor: anInterval 
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	messages 
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages 
				collect: [:each | each bestNodeFor: anInterval]) 
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]