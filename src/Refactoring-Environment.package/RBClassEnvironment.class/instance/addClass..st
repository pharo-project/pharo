adding
addClass: aClass 
	aClass isMeta
		ifTrue: [ metaClasses add: aClass soleInstance name ]
		ifFalse: [ classes add: aClass name ]