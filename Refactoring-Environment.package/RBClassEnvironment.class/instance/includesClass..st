testing
includesClass: aClass 
	^(aClass isMeta
		ifTrue: [metaClasses includes: aClass soleInstance name]
		ifFalse: [classes includes: aClass name])
			and: [super includesClass: aClass]