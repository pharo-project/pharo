"
I present a side-by-side view of a failing assert, with at my left an inspector on the failing object and at my left the expected object.

Only opens for following assertions failures:
#assert:equals: 
#assert:closeTo:precision: 
#assert:closeTo: 
#assert:identicalTo: 
#assertCollection:equals: 
#assertCollection:hasSameElements:
"
Class {
	#name : #StFailingAssertionInspector,
	#superclass : #SpPresenter,
	#instVars : [
		'actualObjectHeader',
		'expectedObjectHeader',
		'actualObject',
		'expectedObject',
		'actualObjectMillerList',
		'expectedObjectMillerList',
		'stringDiff'
	],
	#category : #'NewTools-Debugger-View'
}

{ #category : #specs }
StFailingAssertionInspector class >> defaultSpec [
	^ SpPanedLayout newHorizontal
		  position: 50 percent;
		  add: (SpBoxLayout newVertical
				   add: #actualObjectHeader
				   expand: false
				   fill: false
				   padding: 0;
				   add: #actualObjectMillerList;
				   yourself);
		  add: (SpBoxLayout newVertical
				   add: #expectedObjectHeader
				   expand: false
				   fill: false
				   padding: 0;
				   add: #expectedObjectMillerList;
				   yourself);
		  yourself
]

{ #category : #'instance creation' }
StFailingAssertionInspector class >> diffInspectorsFromActual: actualObject expected: expectedObject application: aStApplication [
	| diffPresenter |
	diffPresenter := self basicNew
		application: aStApplication;
		actualObject: actualObject;
		expectedObject: expectedObject;
		initialize.
		
	self flag: 'Not nice but works. This needs improvement.'.
	(actualObject isString and: [ expectedObject isString ])
		ifTrue: [ diffPresenter buildWithSpec: #stringSpec ].
	^ diffPresenter
]

{ #category : #specs }
StFailingAssertionInspector class >> stringSpec [
	^ SpBoxLayout newVertical
		add: #stringDiff;
		yourself
]

{ #category : #accessing }
StFailingAssertionInspector >> actualObject [
	^ actualObject
]

{ #category : #accessing }
StFailingAssertionInspector >> actualObject: anObject [
	actualObject := anObject
]

{ #category : #accessing }
StFailingAssertionInspector >> actualObjectMillerList [
	^ actualObjectMillerList
]

{ #category : #accessing }
StFailingAssertionInspector >> expectedObject [
	^ expectedObject
]

{ #category : #accessing }
StFailingAssertionInspector >> expectedObject: anObject [
	expectedObject := anObject
]

{ #category : #accessing }
StFailingAssertionInspector >> expectedObjectMillerList [
	^ expectedObjectMillerList
]

{ #category : #initialization }
StFailingAssertionInspector >> initializeMillerList: list forObject: object [
	list
		presenterBlock: [ :aModel | self treeTableInspectionFor: aModel ];
		setRootModel: object 
]

{ #category : #initialization }
StFailingAssertionInspector >> initializePresenters [
	self flag: 'Not nice but works. This needs improvement.'.
	(actualObject isString and: [ expectedObject isString ])
		ifTrue: [ ^ self initializePresentersForString ].
		
	actualObjectHeader := (self instantiate: StHeaderBar)
		label: 'Actual object: ' , (actualObject printStringLimitedTo: 40);
		yourself.
	"actualObjectHeader applyStyleToLabel: 'label.headerError'."
	actualObjectMillerList := SpMillerColumnPresenter new.
	self
		initializeMillerList: actualObjectMillerList
		forObject: actualObject.
	expectedObjectHeader := (self instantiate: StHeaderBar)
		label: 'Expected object: ' , (expectedObject printStringLimitedTo: 40);
		yourself.
	"expectedObjectHeader applyStyleToLabel: 'label.headerSuccess'."
	expectedObjectMillerList := SpMillerColumnPresenter new.
	self
		initializeMillerList: expectedObjectMillerList
		forObject: expectedObject
]

{ #category : #initialization }
StFailingAssertionInspector >> initializePresentersForString [
	
	stringDiff := self newDiff.
	stringDiff showOptions: false.
	stringDiff leftLabel: 'Actual result (left) versus expected result (right)'.
	stringDiff rightLabel: ''.
	stringDiff leftText: actualObject.
	stringDiff rightText: expectedObject
]

{ #category : #accessing }
StFailingAssertionInspector >> stringDiff [
	^stringDiff
]

{ #category : #views }
StFailingAssertionInspector >> treeTableInspectionFor: aModel [
	| rawInspection |
	rawInspection := StRawInspection new.
	^ self newTreeTable
		  roots: aModel value allInspectorNodes;
		  children: [ :node | node children ];
		  addColumn: rawInspection variablesColumn;
		  addColumn: rawInspection valuesColumn;
		  yourself
]
