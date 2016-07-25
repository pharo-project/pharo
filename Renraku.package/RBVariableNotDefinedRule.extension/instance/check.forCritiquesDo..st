*renraku
check: aMethod forCritiquesDo: aCriticBlock

	aMethod literals allButLastDo: [ :literal | 
			(literal isVariableBinding and: [ literal key notNil ])
				ifTrue: [ 
					((Smalltalk globals associationAt: literal key ifAbsent: [  ]) == literal
						or: [ (Undeclared associationAt: literal key ifAbsent: [  ]) == literal ])
						ifFalse: [ 
							(aMethod methodClass bindingOf: literal key) == literal
								ifFalse: [ 
										(literal name = #RFMetaLink) ifFalse: [
											aCriticBlock cull: (ReTrivialCritique
												withAnchor: (ReSearchStringSourceAnchor
													entity: aMethod
													string: literal key)
												by: self
												hint: literal key) ] ] ] ] ].