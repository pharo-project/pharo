"
I'm an example of fast table, who will use a list of methods as data source elements.

self example1.
self example2.
self example3.
self example4.
self example5.

"
Class {
	#name : #FTExampleMethodListDataSource,
	#superclass : #FTSimpleDataSource,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #'instance creation' }
FTExampleMethodListDataSource class >> for: aClass [
	^ self basicNew 
		initializeClass: aClass;
		yourself
]

{ #category : #accessing }
FTExampleMethodListDataSource >> cellColumn: column row: rowIndex [
 
	^ FTCellMorph new
		cellInset: 5;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (self elementAt: rowIndex) name asMorph;
		yourself
]

{ #category : #private }
FTExampleMethodListDataSource >> colorFor: index [
	index odd ifTrue: [ ^ Smalltalk ui theme lightBackgroundColor ].
	^ Smalltalk ui theme backgroundColor
]

{ #category : #private }
FTExampleMethodListDataSource >> iconFor: rowIndex [
	^ (self elementAt: rowIndex) actionOn: self
]

{ #category : #initialization }
FTExampleMethodListDataSource >> initializeClass: aClass [
	self initialize.
	self elements: ((aClass selectors sorted: [ :a :b | a < b ])
		collect: [ :each | FTExampleMethodModel method: aClass>>each ])
]

{ #category : #accessing }
FTExampleMethodListDataSource >> menuColumn: column row: rowIndex [
	| menu method |
	
	rowIndex = 0 ifTrue: [ 
		"I could answer a menu without selection, but in this case I will just answer nil, 
		 which means 'no menu'"
		^ nil ].
	
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
FTExampleMethodListDataSource >> searchText: aString [
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	
	self elements withIndexDo: [ :each :index |
		( each name beginsWith: aString)
			ifTrue: [ ^ { index } ] ].
	^ #()
]
