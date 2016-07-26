actions
excludeClassesNotUnderTestFrom: methods
	classesSelected
		do: [ :class | 
			(class class includesSelector: #classNamesNotUnderTest)
				ifTrue: [ 
					class classNamesNotUnderTest
						do: [ :className | 
							| theClass |
							theClass := Smalltalk globals classNamed: className.
							theClass
								ifNotNil: [ 
									theClass methods do: [ :each | methods remove: each methodReference ifAbsent: [  ] ].
									theClass class methods do: [ :each | methods remove: each methodReference ifAbsent: [  ] ] ] ] ] ]