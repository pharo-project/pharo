Extension { #name : #Trait }

{ #category : #'*FluidClassDefinition' }
Trait class >> << aSymbol [
	^ FluidTraitBuilder new 
		name: aSymbol;
		yourself
]

{ #category : #'*FluidClassDefinition' }
Trait >> asFluidDefinitionString [ 

	^ String streamContents: [ :s |		
		s 
			nextPutAll: 'Trait  << #';
			nextPutAll: name;
			cr.
		
		self hasTraitComposition ifTrue: [
			s 
				tab; 
				nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString ;
				nextPutAll: ';';
				cr ].
		
		self slots ifNotEmpty: [ 
					s tab.
					s nextPutAll: 'slots: '.
					self slotDefinitionStringOn: s.
					s nextPutAll: ';'.
					s cr ].
				
		self tagOn: s.
		
		s 	
			tab;
			nextPutAll: 'package: '; 
			nextPut: $';
			nextPutAll: self package packageName;
			nextPut: $'
		]
]

{ #category : #'*FluidClassDefinition' }
Trait >> definition [
	
	^ Slot showSlotClassDefinition
		ifTrue: [ self asFluidDefinitionString ]
		ifFalse: [ self definitionWithoutSlots ]
	
]

{ #category : #'*FluidClassDefinition' }
Trait class >> definitionFullyExpandedButPackage: aString [
	"This definition propose all the with an empty arguments so that the user can just type and empty elements will be ignored by the builder and parser. Should check that this is true and write tests. 
	"
	"For now I will check here for Slot 
	"
	^ Slot showSlotClassDefinition 
		ifTrue: [
				String streamContents: [ :s |
						s nextPutAll: 'Trait << #TMyTrait'; crtab.
						s nextPutAll: 'uses: {};'; crtab.
						s nextPutAll: 'slots: {};'; crtab. 
						s nextPutAll: 'tag: '''' ;';crtab.
						s nextPutAll: 'package: ''', aString, '''' ] 
					]
		ifFalse: [  
				self definitionWithoutSlots		
				]

]

{ #category : #'*FluidClassDefinition' }
Trait >> definitionWithoutSlots [

	(self instanceSide name == #Trait) ifTrue: [ ^ super definition ].

	^ String streamContents: [ :s | 
		s 
			nextPutAll: 'Trait named: ';
			nextPutAll:	self name printString;
			cr; tab; 
			nextPutAll: ' uses: ';
			nextPutAll: self traitComposition traitCompositionExpression; 
			cr.
		self classLayout visibleSlots ifNotEmpty: [ 
			s tab; nextPutAll: ' slots: ';
			nextPutAll: self slotDefinitionString; cr. ].
		s tab; nextPutAll: ' package: ';
			nextPutAll: self category asString printString
	]
]
