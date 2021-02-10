"
A base icon provider. 
Children of `SpIconProvider` will define concrete ways to access icons.
"
Class {
	#name : #SpIconProvider,
	#superclass : #Object,
	#category : #'Spec2-Core-IconProvider'
}

{ #category : #copying }
SpIconProvider >> , aProvider [

	^ SpCompositeIconProvider new 
		addProvider: self;
		addProvider: aProvider;
		yourself
]

{ #category : #accessing }
SpIconProvider >> iconNamed: aName [

	^ self 
		iconNamed: aName 
		ifAbsent: [ self error: ('Icon {1} not found.' format: { aName })  ]
]

{ #category : #accessing }
SpIconProvider >> iconNamed: aName ifAbsent: aBlock [

	self subclassResponsibility
]
