bugs
overridesSpecialMessage
	| detector |
	detector := self new.
	detector name: 'Overrides a "special" message'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			((context selectedClass isMetaclass
				ifTrue: [self metaclassShouldNotOverride]
				ifFalse: [self classShouldNotOverride]) detect: 
						[:each | 
						context selectedClass superclass notNil and: 
								[(context selectedClass superclass canUnderstand: each)
									and: [context selectedClass includesSelector: each]]]
					ifNone: [nil]) notNil ifTrue: [result addClass: context selectedClass]].
	^detector