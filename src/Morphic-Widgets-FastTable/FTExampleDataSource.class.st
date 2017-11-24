"
I'm a simple example of fast table, who will use a list of classes and traits as data source elements.

FTExamples example1.
FTExamples example2.
"
Class {
	#name : #FTExampleDataSource,
	#superclass : #FTSimpleDataSource,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #accessing }
FTExampleDataSource >> cellColumn: column row: rowIndex [ 
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		cellInset: 5;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (self elementAt: rowIndex) name asStringMorph;
		yourself
]

{ #category : #private }
FTExampleDataSource >> colorFor: index [
	index odd ifTrue: [ ^ Smalltalk ui theme lightBackgroundColor ].
	^ Smalltalk ui theme backgroundColor
]

{ #category : #private }
FTExampleDataSource >> iconFor: index [
	| class |
	class := self elementAt: index.
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	^ class systemIcon
]
