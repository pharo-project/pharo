running
basicCheck: aMethod
	^ aMethod literals allButLast anySatisfy: [ :literal | 
		literal isVariableBinding and: [ literal key notNil and: [ (literal name ~= #RFMetaLink) and: [  
					((Smalltalk globals associationAt: literal key ifAbsent: [  ]) == literal
						or: [ (Undeclared associationAt: literal key ifAbsent: [  ]) == literal ]) not and: [ 
							(aMethod methodClass bindingOf: literal key) ~~ literal ] ] ] ] ]