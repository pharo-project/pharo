preconditions
checkSiblingSuperSendsFrom: aRBClass 
	aRBClass selectors do: 
			[:each | 
			| tree |
			tree := aRBClass parseTreeFor: each.
			tree notNil 
				ifTrue: 
					[tree superMessages do: 
							[:aSelector | 
							(selectors includes: aSelector) 
								ifTrue: 
									[| definer |
									definer := aRBClass superclass whoDefinesMethod: aSelector.
									(definer notNil and: [class includesClass: definer]) 
										ifTrue: 
											[self 
												refactoringError: ('Cannot pull up <1s> since <2p>>><3s> sends a super message to it.' 
														expandMacrosWith: aSelector
														with: aRBClass
														with: each)]]]]].
	aRBClass allSubclasses do: [:each | self checkSiblingSuperSendsFrom: each]