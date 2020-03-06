"
I am the adapter used to bridge a TextPresenter and a PluggableTextMorph
"
Class {
	#name : #SpMorphicTextAdapter,
	#superclass : #SpMorphicBaseTextAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicTextAdapter >> addFocusRotationKeyBindings [
	"Text areas needs to be able to process tab. Cancel the binding."
]

{ #category : #factory }
SpMorphicTextAdapter >> buildWidget [
	| widgetClass newWidget |
	
	self flag: #pharo7.	"When support for P7 will be dropped, remove this compatibility hack."
	widgetClass := self class environment 
		at: #SpRubScrolledTextMorph 
		ifAbsent: [ RubScrolledTextMorph ].
	
	newWidget := (widgetClass on: self)
		getTextSelector: #getText;
		setTextSelector: #accept:notifying:;
		getSelectionSelector: #readSelection;
		menuProvider: self selector: #codePaneMenu:shifted:;
		setSelectionSelector: #setSelection:;
		ghostText: self placeholder;
		beWrapped;
		enabled: self enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		registerScrollChanges: #scrollValueChanged:;
		yourself.
	
	self setEditingModeFor: newWidget.
	
	self presenter whenTextChangedDo: [ :text | self setText: text to: newWidget ].
	self presenter whenPlaceholderChangedDo: [ :text | self setGhostText: text to: newWidget ].
	
	^ newWidget
]

{ #category : #private }
SpMorphicTextAdapter >> setGhostText: text to: aWidget [

	aWidget ghostText: text
]

{ #category : #private }
SpMorphicTextAdapter >> setText: text to: aWidget [

	aWidget setText: text
]
