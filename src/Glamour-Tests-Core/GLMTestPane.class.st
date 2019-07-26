Class {
	#name : #GLMTestPane,
	#superclass : #GLMPane,
	#category : #'Glamour-Tests-Core'
}

{ #category : #private }
GLMTestPane class >> new [

	^super new initialize
]

{ #category : #private }
GLMTestPane class >> portClass [ 
	 
	^GLMTestPort
]

{ #category : #accessing }
GLMTestPane >> browser [

	browser ifNil: [ ^ GLMNoBrowser new ].
	^ browser
]

{ #category : #accessing }
GLMTestPane >> browser: aBrowser [ 
	browser := aBrowser
]
