Class {
	#name : #AbstractPackage,
	#superclass : #Object,
	#instVars : [
		'organizer'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #accessing }
AbstractPackage >> name [

	^ self subclassResponsibility
]

{ #category : #private }
AbstractPackage >> organizer [

	self flag: #package.
	^ organizer ifNil: [ self class organizer ]
]

{ #category : #accessing }
AbstractPackage >> organizer: anObject [

	organizer := anObject
]

{ #category : #accessing }
AbstractPackage >> packageName [

	self flag: #package. "Deprecate and call #name instead."
	^ self name
]
