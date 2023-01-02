"
A source code cleaner
"
Class {
	#name : #ReMethodSourceCleaner,
	#superclass : #Object,
	#instVars : [
		'sourceLines'
	],
	#category : #'Renraku-Utility'
}

{ #category : #testing }
ReMethodSourceCleaner class >> ableToClean: aCompiledMethod [

	^ (self clean: aCompiledMethod sourceCode) size ~= aCompiledMethod sourceCode size
]

{ #category : #cleaning }
ReMethodSourceCleaner class >> clean: aString [

	^(self forSource: aString)
		clean;
		contents
]

{ #category : #'instance creation' }
ReMethodSourceCleaner class >> forSource: aString [

	^ self new
		initFromSource: aString;
		yourself
]

{ #category : #utilities }
ReMethodSourceCleaner class >> showCleaningOf: aString [

	SpDiffPresenter new
 		showOptions: false;
		leftText: aString;
		rightText: (self clean: aString);
		open
]

{ #category : #operating }
ReMethodSourceCleaner >> clean [

	self
		removeTrailingSeparatorLines;
		removeTrailingDot;
		removeTrailingSeparatorsInAllLines
]

{ #category : #accessing }
ReMethodSourceCleaner >> contents [

	^ String streamContents: [:s |
			sourceLines asArray do: [:each | s nextPutAll: each contents ] separatedBy: [ s nextPut: Character cr ]
	]
]

{ #category : #'private - initialization' }
ReMethodSourceCleaner >> initFromSource: aString [

	| lines |
	lines := aString lines withIndexCollect: [ :each :index |
		         ReSourceCodeLine lineNumber: index contents: each ].

	sourceLines := DoubleLinkedList new
		               addAll: lines;
		               yourself
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingDot [

	 sourceLines last removeTrailingDot
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingSeparatorLines [

	[
	sourceLines isNotEmpty and: [
		sourceLines last onlyContainsSeparators ] ] whileTrue: [
		sourceLines removeLast ]
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingSeparatorsInAllLines [

	 sourceLines do: [:each | each removeTrailingSeparators ]
]
