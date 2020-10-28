"
The idea is that we want to have a printer


ClassDefinitionPrinter 
	should be able to return either a specific printer
	or the default one shared globally.
	By default this ithe one we want to use. 

```	
ClassDefinitionPrinter useFluid.

(ClassDefinitionPrinter for: Array) definitionString
```
should return

```
ArrayedCollection < #Array
	layout: VariableLayout;
	tag: ''Base'';
	package: ''Collections-Sequenceable'''
```
	

```
ClassDefinitionPrinter new 
	oldPharo;
	for: Array;
	definitionString
	
ClassDefinitionPrinter new 
	fluid;
	for: Array;
	definitionString
	
ClassDefinitionPrinter new 
	legacy;
	for: Array;
	definitionString
```	

In addition the printer should support 
	- templateDefinitionString
	
	- expandedDefinitionString is the same as definitionString on OldPharo and Legacy.








"
Class {
	#name : #ClassDefinitionPrinter,
	#superclass : #Object,
	#instVars : [
		'forClass'
	],
	#classVars : [
		'ShowFluidClassDefinition'
	],
	#category : #'Kernel-ClassDefinitionPrinter'
}

{ #category : #'instance creation' }
ClassDefinitionPrinter class >> fluid [ 
	^  FluidClassDefinitionPrinter basicNew initialize 
]

{ #category : #'instance creation' }
ClassDefinitionPrinter class >> for: aClass [ 
	^ self new
		for: aClass; 
		yourself

]

{ #category : #'instance creation' }
ClassDefinitionPrinter class >> legacy [
	^ LegacyClassDefinitionPrinter basicNew initialize
]

{ #category : #'instance creation' }
ClassDefinitionPrinter class >> new [

	^ self showFluidClassDefinition
			ifTrue: [ self fluid ]
			ifFalse: [ self oldPharo ]
	
]

{ #category : #'instance creation' }
ClassDefinitionPrinter class >> oldPharo [ 
	^ OldPharoClassDefinitionPrinter basicNew initialize 
]

{ #category : #configure }
ClassDefinitionPrinter class >> showFluidClassDefinition [

	^ ShowFluidClassDefinition ifNil: [ ShowFluidClassDefinition := false ]
]

{ #category : #configure }
ClassDefinitionPrinter class >> showFluidClassDefinition: aBoolean [
 
	ShowFluidClassDefinition := aBoolean
	
]

{ #category : #configure }
ClassDefinitionPrinter class >> toggleShowFluidClassDefinition [

	self showFluidClassDefinition: self showFluidClassDefinition not
]

{ #category : #printing }
ClassDefinitionPrinter >> definitionString [
	"The method is part of the double dispatch. It is an extra starting point. 
	Each entity will select the right definition and call me back.
	Normally this method is not used because the main point is the class...."
	
	^ forClass definitionStringFor: self
]

{ #category : #printing }
ClassDefinitionPrinter >> expandedDefinitionString [
	"The method is part of the double dispatch. It is an extra starting point. 
	Each entity will select the right definition and call me back.
	Normally this method is not used because the main point is the class...."
	
	^ forClass expandedDefinitionStringFor: self
]

{ #category : #accessing }
ClassDefinitionPrinter >> for: aClass [ 
	forClass := aClass
]
