unnecessary code
classNotReferenced
	| detector |
	detector := self new.
	detector name: 'Class not referenced'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			(context selectedClass isMetaclass
				or: [context isApplication
				or: [context selectedClass subclasses isEmpty not]])
					ifFalse: 
						[ context selectedClass allCallsOn ifEmpty:
								[result addClass: context selectedClass;
									addClass: context selectedClass class]]].
	^detector