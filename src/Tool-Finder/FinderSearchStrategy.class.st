"
I am an abstract class to define the behavior of the Finder related to the search strategy used.

I implement a strategy design pattern and my subclasses will manage the different search modes.
"
Class {
	#name : 'FinderSearchStrategy',
	#superclass : 'Object',
	#instVars : [
		'finder'
	],
	#category : 'Tool-Finder-Base',
	#package : 'Tool-Finder',
	#tag : 'Base'
}

{ #category : 'accessing' }
FinderSearchStrategy class >> finder: aFinder [

	^ self new
		  finder: aFinder;
		  yourself
]

{ #category : 'testing' }
FinderSearchStrategy class >> isAbstract [

	^ self = FinderSearchStrategy
]

{ #category : 'accessing' }
FinderSearchStrategy class >> strategyName [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
FinderSearchStrategy class >> strategyNamed: aSymbol [

	^ self subclasses detect: [ :class | class strategyName = aSymbol ]
]

{ #category : 'actions' }
FinderSearchStrategy >> constructDictionary [

	self subclassResponsibility
]

{ #category : 'accessing' }
FinderSearchStrategy >> defaultExplanation [

	^ 'The Finder can be used in 4 different ways:
	- Selectors: your research is done among selectors
	- Classes : your research is done among classes names
	- Source : your research is done among all the source code
	- Pragmas: your research is done among pragmas
	- Examples : your research uses the Method Finder behavior
			   (for further informations, print ''FinderUI methodFinderExplanation'')


In these four modes, you can also tick the ''Use RegEx'' checkbox.
If you pick this box, your search will be done using regular expressions instead of just matching.

The ''Select classes'' button opened a dialog window  to select which classes will be used for the search.
The ''All classes'' button is used to reset the classes selection.'
]

{ #category : 'accessing' }
FinderSearchStrategy >> findSelectedClassIn: path [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
FinderSearchStrategy >> findSelectedMethodIn: path [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
FinderSearchStrategy >> finder [

	^ finder
]

{ #category : 'accessing' }
FinderSearchStrategy >> finder: anObject [

	finder := anObject.
	self updateRegexUsage
]

{ #category : 'testing' }
FinderSearchStrategy >> isClassesSearch [

	^ false
]

{ #category : 'testing' }
FinderSearchStrategy >> isPragmasSearch [

	^ false
]

{ #category : 'actions' }
FinderSearchStrategy >> methodSearch: aSelectBlock [

	| result |
	result := OrderedCollection new.
	aSelectBlock ifNil: [ ^ result ].
	finder packagesSelection classes
		do: [ :class |
			class methodsDo: [ :method | (aSelectBlock value: method) ifTrue: [ result add: method ] ].
			class classSide methodsDo: [ :method | (aSelectBlock value: method) ifTrue: [ result add: method ] ] ]
		displayingProgress: [ :aClass | aClass name ].
	^ result
]

{ #category : 'accessing' }
FinderSearchStrategy >> sourceCodeClass: class method: method [

	^ (class >> method) sourceCode
]

{ #category : 'private' }
FinderSearchStrategy >> updateRegexUsage [

	finder enableUseRegEx
]
