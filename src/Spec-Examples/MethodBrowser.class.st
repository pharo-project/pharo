"
A MethodBrowser is a simple browser using Spec to display a list of methods and their source code

self example
"
Class {
	#name : #MethodBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'listModel',
		'textModel',
		'toolbarModel'
	],
	#category : #'Spec-Examples'
}

{ #category : #specs }
MethodBrowser class >> defaultSpec [
	<spec>
	| delta |
	delta := 15.
	^ SpecLayout composed 
			add: #listModel origin: 0@0 corner: 1@0.5 offsetOrigin: 0@0 offsetCorner: 0@(delta negated);
			add: #toolbarModel origin: 0@0.5 corner: 1@0.5 offsetOrigin: 0@(delta negated) offsetCorner: 0@delta;
			add: #textModel origin: 0@0.5 corner: 1@1 offsetOrigin: 0@delta offsetCorner: 0@0;
			yourself
]

{ #category : #specs }
MethodBrowser class >> defaultSpec2 [
^{ #Panel.
		#changeTableLayout.
		#listDirection:. 	#bottomToTop.
		#addMorph:. 		{#model. #listModel.}.
		#addMorph:. 		{#model. #textModel.}.
		#vResizing:.		 #spaceFill.
		#hResizing:.		 #spaceFill.}
]

{ #category : #example }
MethodBrowser class >> example [
	<sampleInstance>
	| example |
	example := MethodBrowser new.
	example methods: Object methods;
	 openWithSpec.
	^example
]

{ #category : #specs }
MethodBrowser class >> title [

	^ 'Method Browser'
]

{ #category : #accessing }
MethodBrowser >> acceptBlock: aBlock [

	textModel acceptBlock: aBlock
]

{ #category : #accessing }
MethodBrowser >> action [

	^ textModel actionToPerformHolder content
]

{ #category : #api }
MethodBrowser >> displayBlock: aBlock [

	listModel displayBlock: aBlock
]

{ #category : #initialization }
MethodBrowser >> initializePresenter [

	listModel whenSelectedItemChanged: [:selection |
		selection
			ifNil: [
				textModel text: ''.
				textModel behavior: nil.
				toolbarModel method: nil ]
			ifNotNil: [:m | 
				textModel text: m sourceCode.
				textModel behavior: m methodClass.
				toolbarModel method: m ]].
	
	self acceptBlock: [:t | self listModel selectedItem inspect ].
	self wrapWith: [:item | item methodClass name,'>>#', item selector ].
]

{ #category : #initialization }
MethodBrowser >> initializeWidgets [
	listModel := self instantiate: ListPresenter .
	textModel := self newCode.
	toolbarModel := self instantiate: MethodToolbar.
	self focusOrder
		add: listModel;
		add: toolbarModel;
		add: textModel.
	textModel aboutToStyle: true
]

{ #category : #accessing }
MethodBrowser >> listModel [

	^ listModel
]

{ #category : #api }
MethodBrowser >> methods: aList [

	"Here I reroute my entry point to the list model's entry point"
	self listModel items: aList
]

{ #category : #api }
MethodBrowser >> sortingBlock: aBlock [ 

	listModel sortingBlock: aBlock 
]

{ #category : #focus }
MethodBrowser >> takeKeyboardFocus [

	^ self listModel takeKeyboardFocus
]

{ #category : #accessing }
MethodBrowser >> textModel [

	^ textModel
]

{ #category : #accessing }
MethodBrowser >> toolbarModel [

	^ toolbarModel
]

{ #category : #api }
MethodBrowser >> wrapWith: aBlock [

	listModel displayBlock: aBlock
]
