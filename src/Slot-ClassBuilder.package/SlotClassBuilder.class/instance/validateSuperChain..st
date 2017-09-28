private validating
validateSuperChain: oldClass
	superclass ifNil: [ ^ nil ].
	superclass withAllSuperclassesDo: [ :class |
		class == oldClass
			ifTrue: [ CircularHierarchyError signalFor: oldClass ]].