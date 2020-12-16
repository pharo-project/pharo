Extension { #name : #MCTraitDefinition }

{ #category : #'*Ring-Definitions-Monticello' }
MCTraitDefinition >> asRingDefinition [

	| ring |
	ring := ( RGTraitDefinition named: self className)
		category: self category;
		superclassName: self superclassName;  
		traitCompositionSource: self traitCompositionString;
		comment: self comment;
		stamp: self commentStamp;
		definitionSource: self definitionString;
		withMetaclass.
		
	ring classSide 
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self classDefinitionString.
	^ring
]

{ #category : #'*Ring-Definitions-Monticello' }
MCTraitDefinition >> classDefinitionString [
	"Answer a string describing the class-side definition."
	
	^String streamContents: [:stream | self printMetaDefinitionOn: stream]
]

{ #category : #'*Ring-Definitions-Monticello' }
MCTraitDefinition >> printMetaDefinitionOn: stream [

	stream nextPutAll: self className, ' classTrait'; 
		 crtab;
		 nextPutAll: 'uses: ';
		 nextPutAll: self classTraitCompositionString.
	
	self classInstanceVariables ifNotEmpty: [  
		stream
			cr; tab;
			nextPutAll: 'slots: '; 
			nextPutAll: self classSlotDefinitionString
	].	

]
