"
A ClassMethodBrowser is a simple browser using spec and reusing MethodBrowser to browse classes>>methods>>sourceCode.

self example
"
Class {
	#name : #ClassMethodBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'listModel',
		'methodModel'
	],
	#category : #'Spec-Examples'
}

{ #category : #specs }
ClassMethodBrowser class >> defaultSpec [
<spec>
	^ SpecLayout composed
		newRow: [:row | row
						add: #listModel;
						add: #methodModel ];
		yourself
]

{ #category : #specs }
ClassMethodBrowser class >> defaultSpec2 [
<spec: #default>

	^ SpecLayout composed
		newRow: [:r | 
			r 
				newColumn: #listModel right: 0.5;
				addVSplitter;
				newColumn: #(methodModel listModel) left: 0.5 ] bottom: 0.5;
		addHSplitter;
		newRow: #(methodModel textModel) top: 0.5;
		yourself
]

{ #category : #example }
ClassMethodBrowser class >> example [
	| example |
	example := ClassMethodBrowser new.
	example
		classes: Smalltalk allClasses;
		openWithSpec.
		^example
]

{ #category : #specs }
ClassMethodBrowser class >> title [

	^ 'Class Method Browser'
]

{ #category : #api }
ClassMethodBrowser >> classes: aList [

	self listModel items: aList
]

{ #category : #initialization }
ClassMethodBrowser >> initializePresenter [
	
	listModel whenSelectedItemChanged: [:selection |
		selection 
			ifNotNil: [:class | 
				methodModel methods: (class methods sort: [:a :b | a selector < b selector]).
				methodModel listModel resetSelection ]].
]

{ #category : #initialization }
ClassMethodBrowser >> initializeWidgets [
	listModel := self instantiate:  ListPresenter .
	methodModel := self instantiate: MethodBrowser.
	self focusOrder
		add: listModel;
		add: methodModel.
	methodModel displayBlock: [ :method | method selector ]
]

{ #category : #accessing }
ClassMethodBrowser >> listModel [

	^ listModel
]

{ #category : #accessing }
ClassMethodBrowser >> methodModel [

	^ methodModel
]
