possible bugs
returnsBooleanAndOther
	| detector matcher |
	detector := self new.
	detector name: 'Returns a boolean and non boolean'.
	matcher := RBParseTreeSearcher new.
	matcher addSearch: '^``@xObject' -> 
					[:aNode :answer | 
					answer add: aNode value;
						yourself].
	detector methodBlock: 
			[:context :result | 
			| hasBool hasSelf |
			hasBool := false.
			hasSelf := context parseTree lastIsReturn not.
			(matcher executeTree: context parseTree initialAnswer: Set new) do: 
					[:each | 
					hasBool := hasBool or: 
									[(each isLiteralNode and: [{true. false} includes: each value])
										or: [each isMessage and: [#(#and: #or:) includes: each selector]]].
					hasSelf := hasSelf or: 
									[(each isVariable and: [each name = 'self'])
										or: [each isLiteralNode and: [({true. false} includes: each value) not]]]].
			hasSelf & hasBool
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector