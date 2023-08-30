Class {
	#name : #AbstractPackage,
	#superclass : #Object,
	#instVars : [
		'organizer'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #private }
AbstractPackage >> organizer [

	self flag: #package.
	^ organizer ifNil: [ self class organizer ]
]

{ #category : #accessing }
AbstractPackage >> organizer: anObject [

	organizer := anObject
]
