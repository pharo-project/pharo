private
metaTraitDefinition: string with: chgRec
	| tokens trait |
	
	tokens := self scannedSource: string.
	
	trait := self classNamed: tokens first.
	trait isTrait
		ifFalse:[ trait:= self createTraitNamed: tokens first ].
	
	trait theMetaClass definitionSource: string