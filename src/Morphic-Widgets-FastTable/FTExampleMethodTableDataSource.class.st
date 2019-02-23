"
I'm an example of fast table, who will use a list of methods as data source elements.

self example1.
self example2.
"
Class {
	#name : #FTExampleMethodTableDataSource,
	#superclass : #FTSimpleDataSource,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #'instance creation' }
FTExampleMethodTableDataSource class >> for: aClass [
	^ self basicNew 
		initializeClass: aClass;
		yourself
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> cellColumn: column row: rowIndex [ 
	column id = '#' ifTrue: [ ^ FTCellMorph new addMorph: rowIndex asMorph ].
	column id = 'Name' ifTrue: [ ^ self nameColumn: column row: rowIndex ].
	column id = 'Protocol' ifTrue: [ ^ self protocolColumn: column row: rowIndex ].
	column id = 'Origin' ifTrue: [ ^ self originColumn: column row: rowIndex ].
	
	self error: 'Invalid column'
]

{ #category : #private }
FTExampleMethodTableDataSource >> colorFor: index [
	"index odd ifTrue: [ ^ Smalltalk ui theme lightBackgroundColor ]."
	^Color transparent "Smalltalk ui theme backgroundColor"
]

{ #category : #private }
FTExampleMethodTableDataSource >> iconFor: rowIndex [
	^ (self elementAt: rowIndex) actionOn: self
]

{ #category : #initialization }
FTExampleMethodTableDataSource >> initializeClass: aClass [
	self initialize.
	self elements: ((aClass allSelectors sorted: [ :a :b | a < b ]) collect: [ :each | FTExampleMethodModel method: (aClass lookupSelector: each) ])
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> menuColumn: column row: rowIndex [
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].

	"protocols do not have associated menu"
	column id = 'Name' ifTrue: [ ^ self nameMenuColumn: column row: rowIndex ].
	column id = 'Origin' ifTrue: [ ^ self originMenuColumn: column row: rowIndex ].	

	^ nil	
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> nameColumn: column row: rowIndex [
	^ FTCellMorph new  
		color: (self colorFor: rowIndex);
		cellInset: 5;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (self elementAt: rowIndex) name asStringMorph;
		yourself
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> nameMenuColumn: column row: rowIndex [
	| method menu |
	method := (self elementAt: rowIndex) method.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: ('Browse ', (method methodClass name), '>>#', method selector) 
		target: method
		selector: #browse.
	menu add: ('Inspect #', (method methodClass name), '>>#', method selector)
		target: method
		selector: #inspect.

	^ menu
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> originColumn: column row: rowIndex [
	^ FTCellMorph new 
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) origin name asMorph;
		yourself
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> originMenuColumn: column row: rowIndex [
	| method menu |
	method := (self elementAt: rowIndex) method.
	menu := UIManager default newMenuIn: self table for: self.
	menu add: ('Browse ', (method methodClass name)) 
		target: method methodClass
		selector: #browse.

	^ menu
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> protocolColumn: column row: rowIndex [
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) protocol asStringMorph;
		yourself
]

{ #category : #accessing }
FTExampleMethodTableDataSource >> searchText: aString [
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	
	self elements withIndexDo: [ :each :index |
		( each name beginsWith: aString)
			ifTrue: [ ^ { index } ] ].
	^ #()
]
