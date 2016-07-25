unnecessary code
unreferencedVariables
	| detector |
	detector := self new.
	detector name: 'Variables not referenced'.
	detector result: nil unreferenced.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					allSubclasses
						detect: [:class | (class whichSelectorsAccess: each) isEmpty not]
						ifNone: [result addInstVar: each for: context selectedClass]].
			context selectedClass isMetaclass ifFalse: 
					[context selectedClass classPool associationsDo: 
							[:each | 
							(SystemNavigation default allCallsOn: each) ifEmpty:  [result addClassVar: each key for: context selectedClass]]]].
	^detector