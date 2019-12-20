"
I'm an example of fast table, who will use a list of classes as data source elements.

"
Class {
	#name : #FTExampleClassInfoTableDataSource,
	#superclass : #FTSimpleDataSource,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> cellColumn: column row: rowIndex [ 
	column id = 'Icon' ifTrue: [ ^ self iconColumn: column row: rowIndex ].
	column id = 'Name' ifTrue: [ ^ self nameColumn: column row: rowIndex ].
	column id = 'Number of methods' ifTrue: [ ^ self numberOfMethodsColumn: column row: rowIndex ].
	column id = 'Number of instance variables' ifTrue: [ ^ self numberOfInstanceVariablesColumn: column row: rowIndex ].
	
	self error: 'Invalid column'
]

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> iconColumn: column row: rowIndex [
	^ FTCellMorph new  
		addMorphBack: (self iconFor: rowIndex) asMorph;
		yourself
]

{ #category : #private }
FTExampleClassInfoTableDataSource >> iconFor: index [
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

{ #category : #initialization }
FTExampleClassInfoTableDataSource >> initialize [
	super initialize.
	self elements: Smalltalk allClassesAndTraits shuffled
]

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> nameColumn: column row: rowIndex [
	^ FTCellMorph new  
		addMorphBack: (self elementAt: rowIndex) name asStringMorph;
		yourself
]

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> numberOfInstanceVariablesColumn: column row: rowIndex [
	^ FTCellMorph new 
		addMorph: (self elementAt: rowIndex) instSize asStringMorph;
		yourself
]

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> numberOfMethodsColumn: column row: rowIndex [
	^ FTCellMorph new
		addMorph: (self elementAt: rowIndex) numberOfMethods asStringMorph;
		yourself
]

{ #category : #accessing }
FTExampleClassInfoTableDataSource >> searchText: aString [
	aString isEmptyOrNil ifTrue: [ ^ #() ].

	self elements withIndexDo: [ :each :index | (each name beginsWith: aString) ifTrue: [ ^ {index} ] ].
	^ #()
]
