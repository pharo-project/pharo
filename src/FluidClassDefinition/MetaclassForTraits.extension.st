Extension { #name : #MetaclassForTraits }

{ #category : #'*FluidClassDefinition' }
MetaclassForTraits >> definition [
	^ Slot showSlotClassDefinition
		ifTrue: [ self asFluidDefinitionString ]
		ifFalse: [ self definitionWithoutSlots ]
]

{ #category : #'*FluidClassDefinition' }
MetaclassForTraits >> definitionExpanded [
	^ String streamContents: [ :s | 
		s nextPutAll: self instanceSide name;
			nextPutAll: ' classTrait';
			cr; tab.
		self hasTraitComposition 
			ifTrue: [ s 
						nextPutAll: 'uses: ';
						nextPutAll: self traitCompositionString;
						nextPutAll: ';' ]
			ifFalse: [ s nextPutAll: 'uses: {};'].
		s crtab.
		s nextPutAll: 'slots: ', self slotDefinitionString ]
]

{ #category : #'*FluidClassDefinition' }
MetaclassForTraits >> definitionwithoutSlots [
	^ String streamContents: [ :s | 
		s nextPutAll: self instanceSide name;
			nextPutAll: ' classTrait';
			cr; tab;
			nextPutAll: 'uses: ';
			nextPutAll: composition traitCompositionExpression.
			
		self localSlots ifNotEmpty: [ 
				s crtab;
					nextPutAll: 'slots: ';
					nextPutAll: self slotDefinitionString]]
]
