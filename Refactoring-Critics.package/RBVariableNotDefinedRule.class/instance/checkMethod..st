running
checkMethod: aMethod
	aMethod literals allButLastDo: [ :literal | 
			(literal isVariableBinding and: [ literal key notNil ])
				ifTrue: [ 
					((Smalltalk globals associationAt: literal key ifAbsent: [  ]) == literal
						or: [ (Undeclared associationAt: literal key ifAbsent: [  ]) == literal ])
						ifFalse: [ 
							(aMethod methodClass bindingOf: literal key) == literal
								ifFalse: [ 
										(literal name = #RFMetaLink) ifFalse: [  
											result
											addMethod: aMethod;
											addSearchString: literal key ] ] ] ] ]