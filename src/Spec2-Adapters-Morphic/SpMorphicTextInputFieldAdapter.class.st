"
I am the adapter used to bridge a TextInputFieldPresenter and a PluggableTextFieldMorph
"
Class {
	#name : #SpMorphicTextInputFieldAdapter,
	#superclass : #SpMorphicBaseTextAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> accept: aText notifying: aNotifyier [
	"We use #asString because rubric send Text instead of strings."

	super
		accept: aText asString 
		notifying: aNotifyier
]

{ #category : #factory }
SpMorphicTextInputFieldAdapter >> buildWidget [
	| newWidget |
	
	newWidget := (RubTextFieldMorph on: self)
		getTextSelector: #getText;
		setTextSelector: #accept:notifying:;
		getSelectionSelector: #readSelection;
		menuProvider: self selector: #codePaneMenu:shifted:;
		setSelectionSelector: #setSelection:;
		entryCompletion: self entryCompletion;
		ghostText: self getPlaceholderText;
		encrypted: self encrypted;
		enabled: self enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: true;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		maxLength: self presenter maxLength;
		yourself.

	self presenter whenTextChangedDo: [ :text | 
		newWidget setText: text ].
	self presenter whenSelectionChangedDo: [ :selectionInterval | 
		self selectionInterval: selectionInterval ].
	self presenter whenPlaceholderChangedDo: [ :text | 
		newWidget ghostText: (text ifNotNil: [ :aString | 
			aString localizedForPresenter: self presenter ]) ].
	self presenter whenPasswordChangedDo: [ :isPassword | 
		newWidget encrypted: isPassword ].
	self presenter whenMaxLengthChangedDo: [ :length | 
		newWidget maxLength: length ].

	^ newWidget
]

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> encrypted [
	^ self model isPassword
]

{ #category : #'spec protocol' }
SpMorphicTextInputFieldAdapter >> encrypted: aBoolean [

	self widgetDo: [ :w | 
		w encrypted: aBoolean.
		w changed ]
]

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> entryCompletion [

	^ self model entryCompletion
]

{ #category : #private }
SpMorphicTextInputFieldAdapter >> getPlaceholderText [

	^ self presenter placeholder ifNotNil: [ :aString | 
		aString localizedForPresenter: self presenter ]
]

{ #category : #testing }
SpMorphicTextInputFieldAdapter >> isPassword [
	
	^ self widget font isKindOf: FixedFaceFont
]

{ #category : #accessing }
SpMorphicTextInputFieldAdapter >> text: aString [
	widget
		setText: aString;
		acceptTextInModel
]
