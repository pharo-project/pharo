"
I am a UI for test failures which have been serialized as fuel files. You can:
* materialize the failed test process to debug
* browse the test method.

self open. 
"
Class {
	#name : #TestReviver,
	#superclass : #ComposablePresenter,
	#instVars : [
		'errorListModel',
		'debugButton',
		'browseButton',
		'vmVersionLabel',
		'vmVersion',
		'timeStamp',
		'timeStampLabel',
		'imageVersion',
		'imageVersionLabel'
	],
	#category : #'SUnit-UI-Tools'
}

{ #category : #opening }
TestReviver class >> open [
	<script>
	
	|model|
	model := self new.
	model openWithSpec model: model.
	^model
]

{ #category : #specs }
TestReviver class >> spec [
	<spec: #default>

	^ SpecLayout composed
		newColumn: [:c |
			c 
				newRow: [ :spec | spec
					add: #debugButton;
					add: #browseButton ] height: 30;
				newRow: [ :spec | spec add:  #errorListModel ];
				newRow: [ :spec | spec
						add: #timeStampLabel width: 100;
						add: #timeStamp ] height: 30;
				newRow: [ :spec | spec
						add: #imageVersionLabel width: 100;
						add: #imageVersion ] height: 30;
				newRow: [ :spec | spec
						add: #vmVersionLabel width: 100;
						add: #vmVersion ] height: 75];
		yourself.
]

{ #category : #accessing }
TestReviver class >> title [

	^ 'Test Error Browser'.
]

{ #category : #private }
TestReviver >> browse: anAbstractFileReference [

	| tokens testClass |
	anAbstractFileReference ifNil: [ ^self ].
	tokens := anAbstractFileReference base findTokens: '-'.
	testClass := Smalltalk at: tokens first asSymbol.
	Smalltalk tools browser openOnClass: testClass selector: tokens second asSymbol.
]

{ #category : #private }
TestReviver >> browseButton [

	^ browseButton.
]

{ #category : #private }
TestReviver >> debugButton [

	^ debugButton.
]

{ #category : #private }
TestReviver >> errorListModel [

	^ errorListModel.
]

{ #category : #accessing }
TestReviver >> imageVersion [
	^ imageVersion
]

{ #category : #accessing }
TestReviver >> imageVersionLabel [
	^ imageVersionLabel
]

{ #category : #private }
TestReviver >> initialExtent [

	^ 700@500.
]

{ #category : #initialization }
TestReviver >> initializePresenter [

	errorListModel whenSelectedItemChanged: [ :item | | header |
		item 
			ifNil: [ 
				vmVersion text: ''.
				imageVersion text: ''.
				timeStamp text: ''.
				]	
			ifNotNil: [
				header := FLMaterializer materializeHeaderFromFileNamed: item.
				vmVersion text: (header additionalObjectAt: #VM).
				imageVersion text: (header additionalObjectAt: #Image).
				timeStamp text: (header additionalObjectAt: #Timestamp)
				] 
	]
]

{ #category : #initialization }
TestReviver >> initializeWidgets [
	| serializedErrors |
	self instantiateWidgets.
	serializedErrors := self selectTestFailures.
	errorListModel
		items: serializedErrors;
		displayBlock: [ :e | e base copyReplaceAll: '-' with: '>>#' ].
	serializedErrors isEmpty
		ifFalse: [ errorListModel setSelectedIndex: 1 ].
	vmVersionLabel label: 'VM Version'.
	vmVersion
		text: '';
		enabled: false.
	timeStampLabel label: 'Timestamp'.
	timeStamp
		text: '';
		enabled: false.
	imageVersionLabel label: 'Image Version'.
	imageVersion
		text: '';
		enabled: false.
	debugButton
		label: 'Debug';
		action: [ self materialize: errorListModel selectedItem ].
	browseButton
		label: 'Browse';
		action: [ self browse: errorListModel selectedItem ].
	self focusOrder
		add: debugButton;
		add: browseButton;
		add: errorListModel
]

{ #category : #initialization }
TestReviver >> instantiateWidgets [
	errorListModel := self newList.
	debugButton := self newButton.
	browseButton := self newButton.
	vmVersionLabel := self newLabel.
	vmVersion := self newText.
	timeStampLabel := self newLabel.
	timeStamp := self newText.
	imageVersionLabel := self newLabel.
	imageVersion := self newText
]

{ #category : #private }
TestReviver >> materialize: fuelFileReference [
	fuelFileReference ifNil: [ ^self ].
	FLMaterializer materializeFromFileNamed: fuelFileReference fullName
]

{ #category : #accessing }
TestReviver >> selectTestFailures [	
	| files results tokens method | 
	results := OrderedCollection new.
	files := FileLocator imageDirectory filesMatching: '*.fuel'.
	files do: [:aFile |
		tokens := aFile resolve base findTokens: '-'.	
		method := (Smalltalk globals at: tokens first asSymbol ) lookupSelector:  tokens second asSymbol.
		method pragmas isEmpty
			ifTrue: [ results add: aFile ]
			ifFalse:  [
				(method pragmas first keyword = #expectedFailure) 
					ifFalse: [ results add: aFile ].
				 ]
	].
	^ results
]

{ #category : #accessing }
TestReviver >> timeStamp [
	^ timeStamp
]

{ #category : #accessing }
TestReviver >> timeStampLabel [
	^ timeStampLabel
]

{ #category : #accessing }
TestReviver >> vmVersion [

	^ vmVersion.
]

{ #category : #accessing }
TestReviver >> vmVersionLabel [

	^ vmVersionLabel.
]
