Class {
	#name : 'SHStylePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'styleListPresenter',
		'previewPresenter',
		'buttonBarPresenter'
	],
	#category : 'Shout-UI',
	#package : 'Shout',
	#tag : 'UI'
}

{ #category : 'instance creation' }
SHStylePresenter class >> open [
	<script> 

	^ self new open
]

{ #category : 'private' }
SHStylePresenter >> availableStylesPragmas [
	"Answer a <Collection> of <Pragma> each representing a Shout style"

	^ self textStylerClass styleTablePragmas
]

{ #category : 'initialization' }
SHStylePresenter >> codeMorphStyledAs: aStyleName [

	^ RubScrolledTextMorph new
		wrapped: true;
		setText: (self styledMethodWith: aStyleName);
		in: [ :this |
			this textArea
				readOnly: true;
				font: StandardFonts codeFont ];
		yourself
]

{ #category : 'layout' }
SHStylePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 4;
		add: (	SpBoxLayout newLeftToRight 
			spacing: 4;
			add: styleListPresenter expand: false;
			add: previewPresenter;
			yourself);
		add: buttonBarPresenter expand: false;
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializeButtonBar [

	buttonBarPresenter := self newButtonBar
		placeAtEnd;
		add: (self newButton
			label: 'Apply';
			icon: (self iconNamed: #smallOk);
			help: 'Apply the selected style';
			action: [ SHPreferences setStyleTableNamed: self selectedStyleTableName ];
			yourself);
		add: (self newButton
			label: 'Cancel';
			icon: (self iconNamed: #smallCancel);
			action: [ self delete ];
			yourself);
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializePresenters [

	self initializeStyleList.
	self initializePreview.
	self initializeButtonBar.
]

{ #category : 'initialization' }
SHStylePresenter >> initializePreview [

	previewPresenter := self newMorph
		morph: (self codeMorphStyledAs: self styleName);
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializeStyleList [
	"Private - The receiver's style list contains the names of the available code styles in the system"

	styleListPresenter := self newList
		                      headerTitle: 'Styles';
		                      items: self styleNames;
		                      selectItem: self styleName;
		                      whenSelectedItemChangedDo: [ :item | 
			                      previewPresenter morph: (self codeMorphStyledAs: item) ];
		                      yourself.
]

{ #category : 'initialization' }
SHStylePresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		initialExtent: (950 @ 650) scaledByDisplayScaleFactor;
		windowIcon: (self application iconNamed: #smallWindow);
		title: 'Code Styler'.
]

{ #category : 'initialization' }
SHStylePresenter >> sampleMethod [
	"#copyReplaceFrom:to:with:"
	
	^  OrderedCollection >> #withIndexCollect: "OrderedCollection selectors atRandom"
]

{ #category : 'initialization' }
SHStylePresenter >> selectedStyleTableName [
	"Answer a <String> with the currently selected style name in the receiver"
	
	^ styleListPresenter selectedItem
]

{ #category : 'initialization' }
SHStylePresenter >> styleName [
	"Answer a <Collection> of <String> with the available style names"
	
	^ self textStylerClass styleTableName
]

{ #category : 'initialization' }
SHStylePresenter >> styleNames [
	"Answer a <Collection> of <String> with the available style names"
	
	^ self availableStylesPragmas collect: [ :p | p argumentAt: 1 ]
]

{ #category : 'initialization' }
SHStylePresenter >> styledMethodWith: styleName [
	"Answer a <Text> representing the receiver's styled sample method source"

	^ (self textStylerClass newWithAttributeTableNamed: styleName)
	    style: self sampleMethod sourceCode asText
	    ast: self sampleMethod ast.
	
]

{ #category : 'initialization' }
SHStylePresenter >> textStylerClass [

	^ SpMorphicCodeTextStyler
]
