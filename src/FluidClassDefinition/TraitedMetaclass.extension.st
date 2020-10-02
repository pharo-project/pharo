Extension { #name : #TraitedMetaclass }

{ #category : #'*FluidClassDefinition' }
TraitedMetaclass >> asFluidDefinitionString [

	^ String streamContents: 
		[:strm |
		strm nextPutAll: self name.
		
		self hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: self traitComposition ].
		self slots ifNotEmpty: [  
			strm
				crtab;
				nextPutAll: 'slots: '.
			self slotDefinitionStringOn: strm	]]
]

{ #category : #'*FluidClassDefinition' }
TraitedMetaclass >> definition [

	^ Slot showSlotClassDefinition
		ifTrue: [ self asFluidDefinitionString ]
		ifFalse: [ self definitionWithoutSlots ]
]

{ #category : #'*FluidClassDefinition' }
TraitedMetaclass >> definitionWithoutSlots [
	"Refer to the comment in ClassDescription|definition."
	^ String streamContents: [:strm |
			strm print: self.
			self hasTraitComposition ifTrue: [
				strm
					crtab;
					nextPutAll: 'uses: ';
					print: self traitComposition ].
			strm
				crtab;
				nextPutAll: 'instanceVariableNames: ';
				store: self instanceVariablesString ] 
]
