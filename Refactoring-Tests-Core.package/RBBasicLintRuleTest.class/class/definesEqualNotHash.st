possible bugs
definesEqualNotHash
	| detector |
	detector := self new.
	detector name: 'Defines = but not hash'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			((context selectedClass includesSelector: #=)
				and: [(context selectedClass includesSelector: #hash) not])
				ifTrue: [result addClass: context selectedClass]].
	^detector