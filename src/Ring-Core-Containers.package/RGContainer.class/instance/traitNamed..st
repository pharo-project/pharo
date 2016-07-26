lookup by name
traitNamed: traitName
	"Retrieves an RGTraitDefinition object.
	traitName could be theMetaClass name"
	| trait |
	
	^(trait:= self classOrTraitNamed: traitName) isTrait
	ifTrue:[ trait ]
	ifFalse:[ nil ]