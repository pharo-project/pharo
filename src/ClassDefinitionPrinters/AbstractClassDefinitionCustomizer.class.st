"
I am an abstract class to define the API of the definition customizers.

A definition customizer allows to customize the printing of a class definition. It will be used for example by Traits to add traits to the class printing.
"
Class {
	#name : 'AbstractClassDefinitionCustomizer',
	#superclass : 'Object',
	#category : 'ClassDefinitionPrinters',
	#package : 'ClassDefinitionPrinters'
}

{ #category : 'internal' }
AbstractClassDefinitionCustomizer class >> extendedFluidClassDefinitionFor: forClass on: stream [

	self subclassResponsibility
]

{ #category : 'internal' }
AbstractClassDefinitionCustomizer class >> fluidClassDefinitionFor: forClass on: stream [

	self subclassResponsibility
]

{ #category : 'internal' }
AbstractClassDefinitionCustomizer class >> fluidMetaclassDefinitionFor: forClass on: stream [

	self subclassResponsibility
]

{ #category : 'internal' }
AbstractClassDefinitionCustomizer class >> legacyClassDefinitionFor: forClass on: stream [

	self subclassResponsibility
]

{ #category : 'internal' }
AbstractClassDefinitionCustomizer class >> oldClassDefinitionFor: forClass on: stream [

	self subclassResponsibility
]
