Extension { #name : #MCTraitDefinition }

{ #category : #'*Ring-Monticello' }
MCTraitDefinition >> ensureRingDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		| def classTraitDef |
		def := anRGEnvironment ensureTraitNamed: self className asSymbol.
		classTraitDef := anRGEnvironment ensureMetaclassTraitNamed: (self className, ' classTrait') asSymbol.
		def classTrait: classTraitDef.
		classTraitDef baseTrait: def.
		def traitComposition: (self traitCompositionFrom: self traitComposition parent: def).
"		classTraitDef traitComposition: (self traitCompositionFrom: self classTraitComposition parent: classTraitDef)".		
		((self superclassName = #nil) or: [ self superclassName isNil "for traits"])
			ifTrue: [ def superclass: def ]
			ifFalse: [ def superclass: (anRGEnvironment ensureClassNamed: superclassName ) ].
		def category: self category.
		def comment: (
			(RGComment parent: def) 
				content: self comment;
				author: (self authorForStamp: self commentStamp);
				time: (self timeForStamp: self commentStamp);
				yourself).
		def]. 
	
]
