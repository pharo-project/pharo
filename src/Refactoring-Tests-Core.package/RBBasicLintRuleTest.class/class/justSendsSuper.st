unnecessary code
justSendsSuper
	| detector matcher |
	detector := self new.
	detector name: 'Method just sends super message'.
	matcher := RBParseTreeSearcher justSendsSuper.
	detector methodBlock: 
			[:context :result | 
			(context parseTree tag isNil
				and: [matcher executeMethod: context parseTree initialAnswer: false])
					ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector