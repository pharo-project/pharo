Extension { #name : #MCTraitDefinition }

{ #category : #'*Ring2-Monticello' }
MCTraitDefinition >> ensureRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| def classTraitDef |
		def := aRG2Environment ensureTraitNamed: self className asSymbol.
		classTraitDef := aRG2Environment ensureMetaclassTraitNamed: (self className, ' classTrait') asSymbol.
		def classTrait: classTraitDef.
		classTraitDef baseTrait: def.
		def traitComposition: (self traitCompositionFrom: self traitComposition parent: def).
"		classTraitDef traitComposition: (self traitCompositionFrom: self classTraitComposition parent: classTraitDef)".		
		((self superclassName = #nil) or: [ self superclassName isNil "for traits"])
			ifTrue: [ def superclass: def ]
			ifFalse: [ def superclass: (aRG2Environment ensureClassNamed: superclassName ) ].
		def category: self category.
		def comment: (
			(RG2CommentDefinition parent: def) 
				content: self comment;
				author: (self authorForStamp: self commentStamp);
				time: (self timeForStamp: self commentStamp);
				yourself).
		def]. 
	
]
