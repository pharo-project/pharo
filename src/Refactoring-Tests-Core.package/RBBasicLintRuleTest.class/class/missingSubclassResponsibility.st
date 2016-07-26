intention revealing
missingSubclassResponsibility
	| detector |
	detector := self new.
	detector name: 'Method defined in all subclasses, but not in superclass'.
	"detector resultClass: MultiEnvironment."
	detector classBlock: 
			[:context :result | 
			| subs |
			subs := context selectedClass subclasses.
			subs size > 1 & context selectedClass isMetaclass not ifTrue: 
					[| sels |
					sels := Bag new.
					subs do: [:each | sels addAll: each selectors].
					sels asSet do: 
							[:each | 
							((sels occurrencesOf: each) == subs size
								and: [(context selectedClass canUnderstand: each) not]) ifTrue: 
										[| envName |
										envName := context selectedClass name , '>>', each.
										subs do: 
												[:subClass | 
												result addClass: subClass
													selector: each
													into: envName]]]]].
	^detector