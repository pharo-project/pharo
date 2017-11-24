"
Abstract step in a wizard

Instance Variables
	input:		<Object>
	name:		<Object>
	numberOfDisplay:		<Object>
	presentationBlock:		<Object>
	previousStep:		<Object>

input
	- xxxxx

name
	- xxxxx

numberOfDisplay
	- xxxxx

presentationBlock
	- xxxxx

previousStep
	- xxxxx

"
Class {
	#name : #GLMAbstractWizardStep,
	#superclass : #Object,
	#instVars : [
		'name',
		'presentationBlock',
		'previousStep',
		'numberOfDisplay',
		'input'
	],
	#category : #'Glamour-Browsers-Wizards'
}

{ #category : #testing }
GLMAbstractWizardStep >> atBeginning [ 

	^ self numberOfDisplay = 1
]

{ #category : #testing }
GLMAbstractWizardStep >> atEnd [

	^ self size = self numberOfDisplay 
]

{ #category : #accessing }
GLMAbstractWizardStep >> decrementNumberOfDisplay [ 

	self numberOfDisplay: self numberOfDisplay - 1
]

{ #category : #scripting }
GLMAbstractWizardStep >> from: aSymbol [

	self previousStep: aSymbol  
]

{ #category : #accessing }
GLMAbstractWizardStep >> incrementNumberOfDisplay [

	self numberOfDisplay: self numberOfDisplay + 1
]

{ #category : #accessing }
GLMAbstractWizardStep >> input [

	^ input 
]

{ #category : #accessing }
GLMAbstractWizardStep >> input: anObject [

	input := anObject
]

{ #category : #accessing }
GLMAbstractWizardStep >> name [

	^ name ifNil: [name := '']
]

{ #category : #accessing }
GLMAbstractWizardStep >> name: aSymbolOrABlock [

	name := aSymbolOrABlock
]

{ #category : #accessing }
GLMAbstractWizardStep >> numberOfDisplay [
	
	^ numberOfDisplay ifNil: [numberOfDisplay := 0]
]

{ #category : #accessing }
GLMAbstractWizardStep >> numberOfDisplay: aNumber [
	
	numberOfDisplay := aNumber
]

{ #category : #testing }
GLMAbstractWizardStep >> overBeginning [ 

	^ self numberOfDisplay < 1
]

{ #category : #testing }
GLMAbstractWizardStep >> overEnd [

	^ self size < self numberOfDisplay 
]

{ #category : #accessing }
GLMAbstractWizardStep >> presentationBlock [

	^ presentationBlock 
]

{ #category : #accessing }
GLMAbstractWizardStep >> presentationBlock: aBlock [

	presentationBlock :=  aBlock
]

{ #category : #accessing }
GLMAbstractWizardStep >> previousStep [

	^ previousStep ifNil: [previousStep := OrderedCollection new]
]

{ #category : #accessing }
GLMAbstractWizardStep >> previousStep: aSymbol [

	self previousStep add: aSymbol
]

{ #category : #scripting }
GLMAbstractWizardStep >> show: aBlock [

	self presentationBlock: aBlock  
]

{ #category : #accessing }
GLMAbstractWizardStep >> size [

	^ self subclassResponsibility 
]
