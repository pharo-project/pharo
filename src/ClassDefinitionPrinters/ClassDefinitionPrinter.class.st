"
The idea is that we want to have a printer


ClassDefinitionPrinter 
	should be able to return either a specific printer
	or the default one shared globally.
	By default this ithe one we want to use. 

```	
ClassDefinitionPrinter showFluidClassDefinition.

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
	#name : 'ClassDefinitionPrinter',
	#superclass : 'Object',
	#instVars : [
		'forClass'
	],
	#classVars : [
		'Customizers',
		'DisplayEmptySlots'
	],
	#category : 'ClassDefinitionPrinters',
	#package : 'ClassDefinitionPrinters'
}

{ #category : 'accessing' }
ClassDefinitionPrinter class >> addCustomizer: aClass [

	(self customizers includes: aClass) ifFalse: [ self customizers add: aClass ]
]

{ #category : 'accessing' }
ClassDefinitionPrinter class >> customizers [
	"I can be use to add classes to me that will be able to print more informations to the class definition.
	For example, when Traits are in the system, they can add a customizer to print themselves"

	^ Customizers ifNil: [ Customizers := OrderedCollection new ]
]

{ #category : 'configure' }
ClassDefinitionPrinter class >> displayEmptySlots [

	^ DisplayEmptySlots ifNil: [ DisplayEmptySlots := true ]
]

{ #category : 'configure' }
ClassDefinitionPrinter class >> displayEmptySlots: aBoolean [

	DisplayEmptySlots := aBoolean
]

{ #category : 'instance creation' }
ClassDefinitionPrinter class >> fluid [
	^  FluidClassDefinitionPrinter new
]

{ #category : 'instance creation' }
ClassDefinitionPrinter class >> for: aClass [
	"Given the current class definition syntax and the need for the class,
	return the correct printer.
	We only print slots with the fluid class definition."

	^ self fluid
		  for: aClass;
		  yourself
]

{ #category : 'testing' }
ClassDefinitionPrinter class >> isAbstract [

	^ self == ClassDefinitionPrinter
]

{ #category : 'instance creation' }
ClassDefinitionPrinter class >> legacy [
	^ LegacyClassDefinitionPrinter new
]

{ #category : 'instance creation' }
ClassDefinitionPrinter class >> oldPharo [
	^ OldPharoClassDefinitionPrinter new
]

{ #category : 'public api' }
ClassDefinitionPrinter >> classDefinitionString [
	^ self subclassResponsibility
]

{ #category : 'printing' }
ClassDefinitionPrinter >> definitionString [
	"The method is part of the double dispatch. It is an extra starting point.
	Each entity will select the right definition and call me back.
	Normally this method is not used because the main point is the class...."

	^ forClass definitionStringFor: self
]

{ #category : 'printing' }
ClassDefinitionPrinter >> expandedDefinitionString [
	"We do not support expansion for most modes so we shortcut the double dispatch call."

	^ self definitionString
]

{ #category : 'accessing' }
ClassDefinitionPrinter >> for: aClass [
	forClass := aClass
]

{ #category : 'accessing' }
ClassDefinitionPrinter >> forClass [

	^ forClass
]

{ #category : 'public api' }
ClassDefinitionPrinter >> metaclassDefinitionString [
	^ self subclassResponsibility
]
