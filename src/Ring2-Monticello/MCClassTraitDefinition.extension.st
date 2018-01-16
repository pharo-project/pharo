Extension { #name : #MCClassTraitDefinition }

{ #category : #'*Ring2-Monticello' }
MCClassTraitDefinition >> asRing2Definition [

	^ self ensureRing2DefinitionIn: RG2Environment new
]

{ #category : #'*Ring2-Monticello' }
MCClassTraitDefinition >> ensureRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| def traitDef |
		def := aRG2Environment ensureMetaclassTraitNamed: (self baseTrait, ' classTrait') asSymbol.
		traitDef := aRG2Environment ensureTraitNamed: (self baseTrait) asSymbol.
		def baseTrait: traitDef.
		traitDef classTrait: def.
		def traitComposition: (self traitCompositionFrom: self classTraitComposition parent: def).
		traitDef category: self category. "makes no sense for metaclass trait"
		def]. 
	
]
