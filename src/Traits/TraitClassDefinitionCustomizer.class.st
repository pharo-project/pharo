"
I am used to customize the ClassDefinitionPrinter to print informations about traits

"
Class {
	#name : 'TraitClassDefinitionCustomizer',
	#superclass : 'AbstractClassDefinitionCustomizer',
	#category : 'Traits',
	#package : 'Traits'
}

{ #category : 'internal' }
TraitClassDefinitionCustomizer class >> extendedFluidClassDefinitionFor: forClass on: stream [

	stream crtab.
	stream nextPutAll: 'traits: '.
	forClass hasTraitComposition
		ifTrue: [
				stream
					nextPutAll: '{';
					nextPutAll: forClass traitCompositionString;
					nextPutAll: '};' ]
		ifFalse: [ stream nextPutAll: '{};' ]
]

{ #category : 'internal' }
TraitClassDefinitionCustomizer class >> fluidClassDefinitionFor: forClass on: stream [
	"uses: can the last part of a definition so watch out for terminating ;"

	forClass hasTraitComposition ifTrue: [
			"forClass traitComposition innerClass = TEmpty ifTrue: [ ^ self ]."
			"isEmpty traitComposition should probably moved to Trait."
			stream
				crtab;
				nextPutAll: 'traits: {';
				nextPutAll: forClass traitCompositionString;
				nextPutAll: '};' ]
]

{ #category : 'internal' }
TraitClassDefinitionCustomizer class >> fluidMetaclassDefinitionFor: forClass on: stream [
	"uses: can the last part of a definition so watch out for terminating ;"

	forClass hasTraitComposition ifTrue: [
			"forClass traitComposition innerClass = TEmpty ifTrue: [ ^ self ]."
			"isEmpty traitComposition should probably moved to Trait."
			stream
				crtab;
				nextPutAll: 'traits: {';
				nextPutAll: forClass traitCompositionString;
				nextPutAll: '}'.
			forClass slots ifNotEmpty: [ stream nextPutAll: ';' ] ]
]

{ #category : 'class initialization' }
TraitClassDefinitionCustomizer class >> initialize [

	ClassDefinitionPrinter addCustomizer: self
]

{ #category : 'internal' }
TraitClassDefinitionCustomizer class >> legacyClassDefinitionFor: forClass on: stream [

	forClass hasTraitComposition ifTrue: [
			stream
				crtab;
				nextPutAll: 'uses: ';
				nextPutAll: forClass traitCompositionString ]
]

{ #category : 'internal' }
TraitClassDefinitionCustomizer class >> oldClassDefinitionFor: forClass on: stream [

	forClass hasTraitComposition ifTrue: [
			stream
				crtab;
				nextPutAll: 'uses: ';
				nextPutAll: forClass traitCompositionString ]
]
