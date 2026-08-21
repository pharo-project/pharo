Extension { #name : 'FluidClassDefinitionPrinter' }

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> compactTraitDefinitionTemplateInPackage: aPackageName tag: aTagName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Trait << #TMyTrait';
			  crtab;
			  nextPutAll: 'traits: {};';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab.
		  aTagName ifNotNil: [
			  s
				  nextPutAll: 'tag: ';
				  print: aTagName;
				  nextPutAll: ';';
				  crtab ].
		  s
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> expandedTraitClassDefinitionString [

	^ String streamContents: [ :s |
			  s nextPutAll: 'Trait << '.
			  s nextPutAll: self forClass name.

			  self class customizers do: [ :customizer | customizer extendedFluidClassDefinitionFor: self forClass on: s ].
			  s crtab.
			  s nextPutAll: 'slots: '.
			  self slotDefinitionsOn: s ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> expandedTraitDefinitionString [

	^ String streamContents: [ :s |
			  | tag |
			  s nextPutAll: 'Trait'.
			  self msgAndClassNameOn: s.


			  self class customizers do: [ :customizer | customizer extendedFluidClassDefinitionFor: self forClass on: s ].
			  self slotsOn: s.
			  s crtab.

			  self forClass packageTag ifNotNil: [ :t |
					  tag := t name.
					  tag = self forClass package name ifFalse: [
							  s
								  nextPutAll: 'tag: ';
								  nextPut: $';
								  nextPutAll: tag asString;
								  nextPutAll: ''';'.
							  s crtab ] ].

			  s
				  nextPutAll: 'package: ''';
				  nextPutAll: self forClass package name;
				  nextPutAll: '''' ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> expandedTraitedMetaclassDefinitionString [

	^ String streamContents: [ :strm |
			  strm
				  nextPutAll: self forClass superclass name;
				  nextPutAll: ' << ';
				  nextPutAll: self forClass name.
			  self class customizers do: [ :customizer | customizer fluidClassDefinitionFor: self forClass on: strm ].
			  self lastSlotsOn: strm ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> traitDefinitionString [

	^ String streamContents: [ :strm |
			  strm nextPutAll: 'Trait'.
			  self msgAndClassNameOn: strm.
			  self class customizers do: [ :customizer | customizer fluidClassDefinitionFor: self forClass on: strm ].

			  self forClass slots ifNotEmpty: [ self slotsOn: strm ].
			  self tagOn: strm.
			  self packageOn: strm ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> traitDefinitionTemplateInPackage: aPackageName tag: aTagName named: aTraitName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Trait << ';
			  print: aTraitName asSymbol;
			  crtab;
			  nextPutAll: 'traits: {};';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab;
			  nextPutAll: 'tag: ''';
			  nextPutAll: (aTagName ifNil: [ '' ]);
			  nextPutAll: ''' ;';
			  crtab;
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : '*Traits' }
FluidClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ String streamContents: [ :strm |
			  strm
				  nextPutAll: self forClass superclass name;
				  nextPutAll: ' << ';
				  nextPutAll: self forClass name.

			  self class customizers do: [ :customizer | customizer fluidMetaclassDefinitionFor: self forClass on: strm ].
			  self forClass slots ifNotEmpty: [ self lastSlotsOn: strm ] ]
]
