*OpalCompiler-Core
notShadowedTemporaryNamesFrom: aContext
	| ownVariableNames |
	ownVariableNames := self allDefinedVariables collect: [ :var | var asString ].
	^ aContext tempNames difference: ownVariableNames
