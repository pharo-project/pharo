possible bugs
tempVarOverridesInstVar
	| detector matcher vars varName |
	detector := self new.
	detector name: 'Instance variable overridden by temporary variable'.
	matcher := (RBParseTreeSearcher new) addArgumentSearch: '`xxxvar' -> 
								[:aNode :answer | 
								answer or: 
										[varName := aNode name.
										vars includes: varName]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			vars := context instVarNames.
			(matcher executeTree: context parseTree initialAnswer: false) ifTrue: 
					[result addClass: context selectedClass selector: context selector.
					result addSearchString: varName]].
	^detector