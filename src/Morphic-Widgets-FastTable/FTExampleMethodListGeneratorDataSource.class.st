Class {
	#name : #FTExampleMethodListGeneratorDataSource,
	#superclass : #FTExampleMethodListDataSource,
	#instVars : [
		'generator',
		'size'
	],
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #initialization }
FTExampleMethodListGeneratorDataSource >> elementAt: anIndex [

	^ self 
		elementAt: anIndex 
		ifAbsent: [ self error: 'Index not found' ]
]

{ #category : #initialization }
FTExampleMethodListGeneratorDataSource >> elementAt: index ifAbsent: aBlock [

	[ elements size < index ] 
	whileTrue: [ 
		generator atEnd ifTrue: [ ^ aBlock value ].
		elements add: generator next.
		generator atEnd ifTrue: [ size := elements size ] ].
	^ elements at: index
]

{ #category : #testing }
FTExampleMethodListGeneratorDataSource >> hasElementAt: index [

	self elementAt: index ifAbsent: [ ^ false ].
	^ true
]

{ #category : #initialization }
FTExampleMethodListGeneratorDataSource >> initializeClass: aClass [
	
	self initialize.
	"Just a big number"
	size := 10000. 
	"where we are going to place the elements"
	elements := OrderedCollection new. 
	"the generator"
	generator := Generator on: [ :g |
		(aClass selectors sorted: [ :a :b | a < b ]) 
			do: [ :each | g yield: (FTExampleMethodModel method: aClass>>each) ] ]
	

]

{ #category : #accessing }
FTExampleMethodListGeneratorDataSource >> size [

	^ size
]
