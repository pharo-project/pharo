"
AbstractGroup3 is an abstraction of what a group is.
"
Class {
	#name : #Group,
	#superclass : #Object,
	#instVars : [
		'name',
		'readOnly',
		'removable',
		'isFillable'
	],
	#category : 'Nautilus-GroupManager'
}

{ #category : #'instance creation' }
Group class >> cleanUp [

	self unsubscribeExistingGroups.
]

{ #category : #'instance creation' }
Group class >> named: aString [

	^ self new name: aString
]

{ #category : #'instance creation' }
Group class >> new [
	^ self basicNew initialize
]

{ #category : #'instance creation' }
Group class >> unsubscribeExistingGroups [
	"self unsubscribeExistingGroups"
	
	self allSubInstances do: [ :each |  SystemAnnouncer uniqueInstance unsubscribe: each ].
	
]

{ #category : #adding }
Group >> addClass: aClass [
	"It is for the dropping of a class in a group, do not react"
]

{ #category : #accessing }
Group >> beReadOnly [

	readOnly := true
]

{ #category : #accessing }
Group >> categoryName [
	"This is the package name set if a class is intended to be created in this group"

	^ '_UnpackagedPackage'
]

{ #category : #queries }
Group >> classes [

	^ self subclassResponsibility
]

{ #category : #accessing }
Group >> elements [

	^ self subclassResponsibility
]

{ #category : #testing }
Group >> hasClassNamed: aClassName [
	^ self subclassResponsibility
]

{ #category : #private }
Group >> ifAllowed: aBlock [

	^ self ifAllowed: aBlock ifNot: []
]

{ #category : #private }
Group >> ifAllowed: aBlock ifNot: anotherBlock [

	^ self isReadOnly
		ifTrue: anotherBlock
		ifFalse: aBlock
]

{ #category : #initialization }
Group >> initialize [
	super initialize.
	readOnly := false
		
]

{ #category : #accessing }
Group >> isFillable [

	^ isFillable ifNil: [ isFillable := false ]
]

{ #category : #accessing }
Group >> isFillable: aBoolean [

	isFillable := aBoolean
]

{ #category : #testing }
Group >> isReadOnly [

	^ readOnly == true
]

{ #category : #accessing }
Group >> name [

	^ name
]

{ #category : #accessing }
Group >> name: aString [

	self ifAllowed: [ name := aString ]
]

{ #category : #queries }
Group >> packages [ 
	^ (self classes 
		collect: #package 
		as: Set)
		asArray
]

{ #category : #printing }
Group >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' ( ';
		nextPutAll: self name asString;
		nextPutAll: ' )'.
]

{ #category : #testing }
Group >> removable [

	^ removable ifNil: [ removable := true ]
]

{ #category : #accessing }
Group >> removable: aBoolean [

	removable := aBoolean
]
