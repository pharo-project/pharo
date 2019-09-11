"
A GLMImplicitBrowser is an abstract implementation of a browser that defines an implicit flow of transmissions.

When using implicit browsers, the developer does not have access to the internal transmissions.
"
Class {
	#name : #GLMImplicitBrowser,
	#superclass : #GLMBrowser,
	#instVars : [
		'transmission'
	],
	#category : #'Glamour-Core'
}

{ #category : #accessing }
GLMImplicitBrowser >> addPresentation: aPresentation [
	self transmission transmissionStrategy addPresentation: aPresentation 
]

{ #category : #accessing }
GLMImplicitBrowser >> newTransmission [
	^ self transmission copy
]

{ #category : #scripting }
GLMImplicitBrowser >> show: aBlock [
	self transmission transmissionStrategy presentationsFactory: aBlock
]

{ #category : #accessing }
GLMImplicitBrowser >> transmission [
	^ transmission ifNil: [
		transmission := GLMTransmission new ensureReplacePresentationsStrategy.
		transmission ]
]
