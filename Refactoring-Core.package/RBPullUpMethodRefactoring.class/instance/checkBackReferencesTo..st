preconditions
checkBackReferencesTo: aSelector 
	| definingClass pushUpParseTree |
	definingClass := class superclass whoDefinesMethod: aSelector.
	definingClass isNil ifTrue: [^self].
	pushUpParseTree := class parseTreeFor: aSelector.
	class superclass allSubclasses do: 
			[:each | 
			each selectors do: 
					[:sel | 
					| parseTree |
					parseTree := each parseTreeFor: sel.
					(parseTree notNil and: 
							[(parseTree superMessages includes: aSelector) 
								and: [definingClass == (each whoDefinesMethod: aSelector)]]) 
						ifTrue: 
							[removeDuplicates := true.
							(aSelector == sel 
								and: [parseTree equalTo: pushUpParseTree exceptForVariables: #()]) 
									ifFalse: 
										[self 
											refactoringError: ('Cannot pull up <1s> since it would override the method defined in <2p>' 
													expandMacrosWith: aSelector
													with: definingClass)]]]]