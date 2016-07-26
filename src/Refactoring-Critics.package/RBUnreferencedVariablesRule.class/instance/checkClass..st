running
checkClass: aClass 
	| allSubclasses |
	allSubclasses := aClass withAllSubclasses.
	aClass instVarNames do: 
		[ :each | 
		allSubclasses 
			detect: [ :class | (class whichSelectorsAccess: each) isEmpty not ]
			ifNone: 
				[ result 
					addClass: aClass
					instanceVariable: each ] ].
	aClass isMeta ifFalse: 
		[ aClass classPool associationsDo: 
			[ :each | 
			(SystemNavigation default allCallsOn: each) ifEmpty: 
				[ result 
					addClass: aClass
					classVariable: each key ] ] ]