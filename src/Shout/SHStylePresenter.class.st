"
Provides a Spec presenter to preview how a Shout code style would be displayed in the system.
It also allows you to apply the selected style.

```
SHStylePresenter open.
```
"
Class {
	#name : 'SHStylePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'styleListPresenter',
		'buttonBarPresenter',
		'classDefinitionPreviewPresenter',
		'methodPreviewPresenter'
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

{ #category : 'private' }
SHStylePresenter >> classDefinitionTextStyledAs: aStyleName [

	^ RubScrolledTextMorph new
		wrapped: true;
		setText: (self styledClassDefinitionWith: aStyleName);
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
			add: (SpPanedLayout newTopToBottom 
				add: classDefinitionPreviewPresenter;
				add: methodPreviewPresenter;
				yourself);
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
SHStylePresenter >> initializeClassDefinitionPreview [

	classDefinitionPreviewPresenter := self newMorph
		morph: (self classDefinitionTextStyledAs: self styleName);
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializeMethodPreview [

	methodPreviewPresenter := self newMorph
		morph: (self methodTextStyledAs: self styleName);
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializePresenters [

	self initializeStyleList.
	self initializeMethodPreview.
	self initializeClassDefinitionPreview.
	self initializeButtonBar.
]

{ #category : 'initialization' }
SHStylePresenter >> initializeStyleList [
	"Private - The receiver's style list contains the names of the available code styles in the system"

	styleListPresenter := self newList
		                      headerTitle: 'Styles';
		                      items: self styleNames;
		                      selectItem: self styleName;
		                      whenSelectedItemChangedDo: [ :item | self updatePreviewPresenters: item ];
		                      yourself.
]

{ #category : 'initialization' }
SHStylePresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		initialExtent: (950 @ 650) scaledByDisplayScaleFactor;
		windowIcon: (self application iconNamed: #smallWindow);
		title: 'Code Styler'.
]

{ #category : 'private' }
SHStylePresenter >> methodTextStyledAs: aStyleName [

	^ RubScrolledTextMorph new
		wrapped: true;
		setText: (self styledMethodWith: aStyleName);
		in: [ :this |
			this textArea
				readOnly: true;
				font: StandardFonts codeFont ];
		yourself
]

{ #category : 'private' }
SHStylePresenter >> sampleClass [

	^ OrderedCollection 
]

{ #category : 'private' }
SHStylePresenter >> sampleMethod [
	"#copyReplaceFrom:to:with:"
	
	^  self sampleClass >> #withIndexCollect: "OrderedCollection selectors atRandom"
]

{ #category : 'private' }
SHStylePresenter >> selectedStyleTableName [
	"Answer a <String> with the currently selected style name in the receiver"
	
	^ styleListPresenter selectedItem
]

{ #category : 'private' }
SHStylePresenter >> styleName [
	"Answer a <Collection> of <String> with the available style names"
	
	^ self textStylerClass styleTableName
]

{ #category : 'private' }
SHStylePresenter >> styleNames [
	"Answer a <Collection> of <String> with the available style names"
	
	^ self availableStylesPragmas collect: [ :p | p argumentAt: 1 ]
]

{ #category : 'private' }
SHStylePresenter >> styledClassDefinitionWith: styleName [
	"Answer a <Text> representing the receiver's styled sample class definition"

	^ (self textStylerClass newWithAttributeTableNamed: styleName)
			style: self sampleClass definitionString asText;
			text
	
]

{ #category : 'private' }
SHStylePresenter >> styledMethodWith: styleName [
	"Answer a <Text> representing the receiver's styled sample method source"

	^ (self textStylerClass newWithAttributeTableNamed: styleName)
	    style: self sampleMethod sourceCode asText
	    ast: self sampleMethod ast.
	
]

{ #category : 'private' }
SHStylePresenter >> textStylerClass [

	^ SpMorphicCodeTextStyler
]

{ #category : 'callbacks' }
SHStylePresenter >> updatePreviewPresenters: item [

	classDefinitionPreviewPresenter morph: (self classDefinitionTextStyledAs: item).
	methodPreviewPresenter morph: (self methodTextStyledAs: item)
]
