possible bugs
collectionCopyEmpty
	| detector |
	detector := self new.
	detector
		name: 'Subclass of collection that has instance variable but doesn''t define copyEmpty'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			(context selectedClass isVariable 
					and: [(context selectedClass includesSelector: #copyEmpty:) not 
						and: [context selectedClass instVarNames isEmpty not
							and: [context selectedClass inheritsFrom: Collection]]])
				ifTrue: [result addClass: context selectedClass]].
	^detector