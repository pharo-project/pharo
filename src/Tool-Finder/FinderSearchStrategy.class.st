Class {
	#name : #FinderSearchStrategy,
	#superclass : #Object,
	#instVars : [
		'finder'
	],
	#category : #'Tool-Finder-Base'
}

{ #category : #accessing }
FinderSearchStrategy class >> finder: aFinder [

	^ self new
		  finder: aFinder;
		  yourself
]

{ #category : #testing }
FinderSearchStrategy class >> isAbstract [

	^ self = FinderSearchStrategy
]

{ #category : #accessing }
FinderSearchStrategy class >> strategyName [

	^ self subclassResponsibility
]

{ #category : #accessing }
FinderSearchStrategy class >> strategyNamed: aSymbol [

	^ self subclasses detect: [ :class | class strategyName = aSymbol ]
]

{ #category : #actions }
FinderSearchStrategy >> constructDictionary [

	self subclassResponsibility
]

{ #category : #accessing }
FinderSearchStrategy >> findSelectedClassIn: path [

	^ self subclassResponsibility
]

{ #category : #accessing }
FinderSearchStrategy >> findSelectedMethodIn: path [

	^ self subclassResponsibility
]

{ #category : #accessing }
FinderSearchStrategy >> finder [

	^ finder
]

{ #category : #accessing }
FinderSearchStrategy >> finder: anObject [

	finder := anObject.
	self updateRegexUsage
]

{ #category : #testing }
FinderSearchStrategy >> isClassesSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isExamplesSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isPragmasSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isSelectorsSearch [

	^ false
]

{ #category : #testing }
FinderSearchStrategy >> isSourceSearch [

	^ false
]

{ #category : #actions }
FinderSearchStrategy >> methodSearch: aSelectBlock [

	| result |
	result := OrderedCollection new.
	aSelectBlock ifNil: [ ^ result ].
	finder packagesSelection classesAndTraits
		do: [ :class |
			class methodsDo: [ :method | (aSelectBlock value: method) ifTrue: [ result add: method ] ].
			class classSide methodsDo: [ :method | (aSelectBlock value: method) ifTrue: [ result add: method ] ] ]
		displayingProgress: [ :aClass | aClass name ].
	^ result
]

{ #category : #accessing }
FinderSearchStrategy >> rootNodeClassForResult: aCollection [

	^ self subclassResponsibility
]

{ #category : #private }
FinderSearchStrategy >> updateRegexUsage [

	finder enableUseRegEx
]
