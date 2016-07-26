private-extensions-string
=> anObject
	<flExtensionOf: #String>	
	^ anObject isBehavior
		ifTrue: [(Smalltalk globals at: #ClassBinding) key: self value: anObject]
		ifFalse: [(Smalltalk globals at: #Global) key: self value: anObject]