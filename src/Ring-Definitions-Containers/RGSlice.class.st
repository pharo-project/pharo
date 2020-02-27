"
A slice is an arbitrary container of elements (working unit)
By inheritance it knows how to treat classes and methods

"
Class {
	#name : #RGSlice,
	#superclass : #RGContainer,
	#category : #'Ring-Definitions-Containers-Base'
}

{ #category : #'adding/removing' }
RGSlice >> addElement: aRGDefinition in: aCollection [

	aRGDefinition environment: self.
	super addElement: aRGDefinition in: aCollection
]

{ #category : #testing }
RGSlice >> isSlice [

	^true
]
