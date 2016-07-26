querying
whoDefines: aName 
	^(self defines: aName) 
		ifTrue: [self]
		ifFalse: [parent notNil ifTrue: [parent whoDefines: aName] ifFalse: [nil]]