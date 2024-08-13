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
		'methodPreviewPresenter',
		'themeListPresenter'
	],
	#category : 'Shout-UI',
	#package : 'Shout-UI'
}

{ #category : 'world menu' }
SHStylePresenter class >> descriptionText [

	^ 'A tool to select the code style for the system'

]

{ #category : 'world menu' }
SHStylePresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Code Styler')
		action: [ self open ];
		order: 35;
		parent: #Tools;
		icon: (self iconNamed: #configuration);
		help: self descriptionText
]

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

{ #category : 'callbacks' }
SHStylePresenter >> classDefinitionText [

	^ RubScrolledTextMorph new
		wrapped: true;
		setText: (self styledClassDefinitionWith: self selectedStyleTableName);
		color: (self themeBackgroundColor: themeListPresenter selectedItem);
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
		add: (	SpPanedLayout newLeftToRight 
			positionOfSlider: 20 percent;
			add: (SpPanedLayout newTopToBottom
				positionOfSlider: 30 percent;
				add: themeListPresenter;
				add: styleListPresenter;
				yourself);
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
			action: [ self updatePreferences ];
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
		morph: self classDefinitionText;
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializeMethodPreview [

	methodPreviewPresenter := self newMorph
		morph: self methodText;
		yourself
]

{ #category : 'initialization' }
SHStylePresenter >> initializePresenters [

	self 
		initializeThemeList;
		initializeStyleList;
		initializeMethodPreview;
		initializeClassDefinitionPreview;
		initializeButtonBar.
]

{ #category : 'initialization' }
SHStylePresenter >> initializeStyleList [
	"Private - The receiver's style list contains the names of the available code styles in the system"

	styleListPresenter := self newList
		                      headerTitle: 'Styles';
		                      items: self styleNames;
		                      selectItem: self styleName;
		                      whenSelectedItemChangedDo: [ :item | self updatePreview ];
		                      yourself.
]

{ #category : 'initialization' }
SHStylePresenter >> initializeThemeList [
	"Private - The receiver's theme list contains the names of the available theme styles in the system"

	themeListPresenter := self newList.
	themeListPresenter model comparisonStrategy: [ : t1 : t2 | t1 class themeName = t2 class themeName ].
	themeListPresenter
		headerTitle: 'Themes';
		items: self themePrototypes;
		display: [ : aTheme | aTheme class themeName ];
		selectItem: self theme;
		whenSelectedItemChangedDo: [ :item | self updatePreview ].
]

{ #category : 'initialization' }
SHStylePresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		initialExtent: (950 @ 650) scaledByDisplayScaleFactor;
		windowIcon: (self application iconNamed: #smallWindow);
		title: 'Code Styler'.
]

{ #category : 'callbacks' }
SHStylePresenter >> methodText [

	^ RubScrolledTextMorph new
		wrapped: true;
		setText: (self styledMethodWith: self selectedStyleTableName);
		color: (self themeBackgroundColor: themeListPresenter selectedItem);
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

{ #category : 'private' }
SHStylePresenter >> themeBackgroundColor: aSelectedItemOrNil [

	^ (aSelectedItemOrNil
		ifNil: [ self theme ]
		ifNotNil: [ themeListPresenter selectedItem ]) backgroundColor
]

{ #category : 'private' }
SHStylePresenter >> themePrototypes [
	"Answer a <Collection> of available themes in the system"

	^ UITheme allThemeClasses collect: [ : th | th new ]
]

{ #category : 'callbacks' }
SHStylePresenter >> updatePreferences [
	"Private - Callback for updating global preferences"
	
	UITheme current: themeListPresenter selectedItem.
	SHPreferences setStyleTableNamed: self selectedStyleTableName.
]

{ #category : 'callbacks' }
SHStylePresenter >> updatePreview [

	classDefinitionPreviewPresenter morph: self classDefinitionText.
	methodPreviewPresenter morph: self methodText
]
