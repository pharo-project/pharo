"
A ClassMethodBrowser is a simple browser using spec and reusing MethodBrowser to browse classes>>methods>>sourceCode.

self example
"
Class {
	#name : #SpClassMethodBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'methodListPresenter',
		'textPresenter',
		'classListPresenter'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpClassMethodBrowser class >> defaultSpec [
	^ SpPanedLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add: #classListPresenter;
				add: #methodListPresenter;
				yourself);
		add: #textPresenter;
		yourself
]

{ #category : #example }
SpClassMethodBrowser class >> example [
	<sampleInstance>
	| example |
	example := SpClassMethodBrowser new.
	example
		classes: self environment allClasses;
		openWithSpec.
	^ example
]

{ #category : #specs }
SpClassMethodBrowser class >> title [

	^ 'Class Method Browser'
]

{ #category : #api }
SpClassMethodBrowser >> classes: aList [
	classListPresenter items = aList ifTrue: [ ^ self ].

	classListPresenter
		items: aList;
		selectIndex: 1
]

{ #category : #initialization }
SpClassMethodBrowser >> connectPresenters [
	classListPresenter transmitTo: methodListPresenter transform: [ :class | class methods sort: #selector descending ] postTransmission: [ :destination | destination selectIndex: 1 ].

	methodListPresenter
		transmitTo: textPresenter
		transform: [ :method | method ifNil: [ '' ] ifNotNil: #sourceCode ]
		postTransmission: [ :destination :origin :transmited | transmited ifNotNil: [ destination behavior: transmited methodClass ] ]
]

{ #category : #initialization }
SpClassMethodBrowser >> initializePresenters [
	classListPresenter := self newList.
	methodListPresenter := self newList.
	textPresenter := self newCode.
	textPresenter
		acceptBlock: [ :t | methodListPresenter selectedItem inspect ].
	methodListPresenter display: #selector.
	self focusOrder
		add: classListPresenter;
		add: methodListPresenter;
		add: textPresenter
]
