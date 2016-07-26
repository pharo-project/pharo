reading
classInstVarNamesFor: aRingClass
	| tokens |
	
	self flag: #traits.
	aRingClass theMetaClass hasDefinitionSource ifFalse: [^ #()].
	tokens := aRingClass theMetaClass definitionSource parseLiterals.
	"tokens size = 4 ifFalse: [self error: 'Unrecognized metaclass definition']."
	^ tokens last findTokens: ' '