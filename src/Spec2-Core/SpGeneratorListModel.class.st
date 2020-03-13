Class {
	#name : #SpGeneratorListModel,
	#superclass : #SpCollectionListModel,
	#instVars : [
		'generator',
		'size'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #example }
SpGeneratorListModel class >> example [
	| items generator |

	items := ($a to: $z) asArray.
	generator := Generator on: [ :g | items do: [ :each | 
		each crTrace.
		g yield: each ] ].

	SpListPresenter new 
		model: (self new 
			generator: generator;
			yourself);
		openWithSpec.

]

{ #category : #accessing }
SpGeneratorListModel >> at: anIndex [

	^ self 
		at: anIndex 
		ifAbsent: [ self error: 'Index not found' ]
]

{ #category : #accessing }
SpGeneratorListModel >> at: anIndex ifAbsent: aBlock [

	[ collection size < anIndex ] 
	whileTrue: [ 
		self generator atEnd ifTrue: [ ^ aBlock value ].
		collection add: self generator next.
		self generator atEnd ifTrue: [ size := collection size ] ].
	^ collection at: anIndex
]

{ #category : #accessing }
SpGeneratorListModel >> generator [
	^ generator
]

{ #category : #accessing }
SpGeneratorListModel >> generator: aGenerator [

	generator := aGenerator.
]

{ #category : #testing }
SpGeneratorListModel >> hasElementAt: index [

	self at: index ifAbsent: [ ^ false ].
	^ true
]

{ #category : #initialization }
SpGeneratorListModel >> initialize [

	super initialize.
	self collection: OrderedCollection new.
	size := 10000
]

{ #category : #accessing }
SpGeneratorListModel >> size [

	^ size
]
