instance creation
createParseTreeRule: codeStrings method: aBoolean name: aName 
	| detector matcher |
	detector := self new.
	detector name: aName.
	matcher := self createMatcherFor: codeStrings method: aBoolean.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector