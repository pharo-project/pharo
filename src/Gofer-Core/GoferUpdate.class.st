"
Update
"
Class {
	#name : #GoferUpdate,
	#superclass : #GoferWorking,
	#category : 'Gofer-Core'
}

{ #category : #private }
GoferUpdate >> addReference: aReference [
	super addReference: aReference.
	self model addVersion: ((self referenceFor: aReference)
		 resolveWith: self gofer) version
]

{ #category : #private }
GoferUpdate >> defaultModel [
	^ MCVersionLoader new
]

{ #category : #running }
GoferUpdate >> execute [
	self model hasVersions
		ifTrue: [ self model load ].
	self gofer cleanup
]

{ #category : #private }
GoferUpdate >> referenceFor: aReference [
	^ aReference
]
