"
A SpecWrapperTableLayout is a variant of SpecTableLayout that is a  wrapper around a single element.
It is used to set hResizing, vResizing, and/or some other properties affecting layout.

"
Class {
	#name : #SpecWrapperTableLayout,
	#superclass : #SpecTableLayout,
	#instVars : [
		'elementAdded'
	],
	#category : #'Spec-Layout'
}

{ #category : #'instance creation' }
SpecWrapperTableLayout class >> column [
	self shouldNotImplement
]

{ #category : #'instance creation' }
SpecWrapperTableLayout class >> row [
	self shouldNotImplement
]

{ #category : #'instance creation' }
SpecWrapperTableLayout class >> wrap: aSpec [
	^ self new add: aSpec
]

{ #category : #'instance creation' }
SpecWrapperTableLayout class >> wrap: modelGetter withSpec: aSymbol [ 
	^ self new add: modelGetter withSpec: aSymbol
]

{ #category : #commands }
SpecWrapperTableLayout >> add: aSpec [
	elementAdded 
		ifTrue: [ Error signal: ('{0} can only hold 1 element' format: { self class name }) ]
		ifFalse: [ super add: aSpec. elementAdded := true. ]
]

{ #category : #commands }
SpecWrapperTableLayout >> add: modelGetter withSpec: aSymbol [ 
	elementAdded 
		ifTrue: [ Error signal: ('{0} can only hold 1 element' format: { self class name }) ]
		ifFalse: [ super add: modelGetter withSpec: aSymbol. elementAdded := true. ]
]

{ #category : #commands }
SpecWrapperTableLayout >> add: modelGetter withSpec: aSymbol wrapped: aBlock [
	self shouldNotImplement
]

{ #category : #commands }
SpecWrapperTableLayout >> add: aSpec wrapped: aBlock [
	self shouldNotImplement
]

{ #category : #commands }
SpecWrapperTableLayout >> addSpacer [
	self shouldNotImplement
]

{ #category : #commands }
SpecWrapperTableLayout >> addSpacerSized: aNumber [
	self shouldNotImplement
]

{ #category : #commands }
SpecWrapperTableLayout >> addSpacerWeighted: aNumber [
	self shouldNotImplement
]

{ #category : #initialization }
SpecWrapperTableLayout >> initialize [
	elementAdded := false.
	super initialize
]
