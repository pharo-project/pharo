testing
compareVariableTypeOf: x withVariable: y in: aContext

	^ (self variableTypeOf: x in: aContext) < (self variableTypeOf: y in: aContext)