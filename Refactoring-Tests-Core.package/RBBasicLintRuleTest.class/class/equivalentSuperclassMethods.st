unnecessary code
equivalentSuperclassMethods
	| detector |
	detector := self new.
	detector name: 'Methods equivalently defined in superclass'.
	detector methodBlock: 
			[:context :result | 
			context selectedClass superclass notNil ifTrue: 
					[(context selectedClass superclass canUnderstand: context selector)
						ifTrue: 
							[(((context selectedClass superclass
								whichClassIncludesSelector: context selector)
									compiledMethodAt: context selector)
									equivalentTo: context compiledMethod)
									ifTrue: [result addClass: context selectedClass selector: context selector]]]].
	^detector