private
privateSelectorsForClass: aClass 
	^aClass isMeta 
		ifTrue: [metaClassSelectors at: aClass soleInstance name ifAbsent: [#()]]
		ifFalse: [classSelectors at: aClass name ifAbsent: [#()]]