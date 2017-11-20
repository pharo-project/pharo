"
The abstract strategy that deals with setting of presentations on the pane of the destination port.
"
Class {
	#name : #GLMPresentStrategy,
	#superclass : #GLMTransmissionStrategy,
	#instVars : [
		'presentationsFactory'
	],
	#category : #Glamour-Core
}

{ #category : #accessing-convenience }
GLMPresentStrategy >> addPresentation: aPresentation [
	|currentPresentations|
	
	currentPresentations  := self presentations.
	self presentationsFactory: [ :composite | 
		currentPresentations do: [ :each | composite add: each ].
		composite add: aPresentation copy ]
]

{ #category : #accessing-convenience }
GLMPresentStrategy >> addPresentations: aCollection [
	aCollection do: [:each | self addPresentation: each ]
]

{ #category : #accessing }
GLMPresentStrategy >> presentations [
	<debuggingStone: #presentationCreation label: 'Presentation creation'>
	| presentations |
	presentations := GLMCompositePresentation new.
	self presentationsFactory glamourValue: (
			presentations asGlamorousMultiValue, 
			(self transmission origins collect: #value )).
	^ presentations
]

{ #category : #accessing }
GLMPresentStrategy >> presentations: anObject [
	"self presentationsFactory: anObject "
]

{ #category : #accessing }
GLMPresentStrategy >> presentationsFactory [
	^ presentationsFactory
]

{ #category : #accessing }
GLMPresentStrategy >> presentationsFactory: anObject [
	presentationsFactory := anObject
]
