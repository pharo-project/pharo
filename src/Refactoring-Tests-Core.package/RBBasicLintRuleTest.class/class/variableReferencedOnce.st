unnecessary code
variableReferencedOnce
	| detector |
	detector := self new.
	detector 
		name: 'Variable referenced in only one method and always assigned first'.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| defClass selector |
					(allSubclasses inject: 0
						into: 
							[:sum :class | 
							| sels |
							sels := class whichSelectorsAccess: each.
							sels size == 1 
								ifTrue: 
									[selector := sels asArray first.
									defClass := class].
							sum + sels size]) 
							== 1 
							ifTrue: 
								[| tree |
								tree := defClass parseTreeFor: selector.
								tree notNil 
									ifTrue: 
										[(RBReadBeforeWrittenTester readBeforeWritten: (Array with: each) in: tree) 
											isEmpty 
												ifTrue: 
													[result addClass: defClass selector: selector.
													result addSearchString: each]]]]].
	^detector