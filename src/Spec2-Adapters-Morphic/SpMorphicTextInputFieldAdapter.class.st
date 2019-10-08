"
I am the adapter used to bridge a TextInputFieldPresenter and a PluggableTextFieldMorph
"
Class {
	#name : #SpMorphicTextInputFieldAdapter,
	#superclass : #SpMorphicBaseTextAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> accept: aText [

	self model accept: aText
]

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> accept: aText notifying: aNotifyier [
	"We use #asString because rubric send Text instead of strings."

	super accept: aText asString notifying: aNotifyier
]

{ #category : #'widget API' }
SpMorphicTextInputFieldAdapter >> acceptOnCR [

	^ self model acceptOnCR
]

{ #category : #factory }
SpMorphicTextInputFieldAdapter >> buildWidget [
	| newWidget |
	
	self flag: #pharo7.	"When support for P7 will be dropped, remove this compatibility hack."
	newWidget := ((self class environment 
			at: #SpRubTextFieldMorph 
			ifAbsent: [ RubTextFieldMorph ]) 
			on: self)
		getTextSelector: #getText;
		setTextSelector: #accept:notifying:;
		getSelectionSelector: #readSelection;
		menuProvider: self selector: #codePaneMenu:shifted:;
		setSelectionSelector: #setSelection:;
		entryCompletion: self entryCompletion;
		ghostText: self placeholder;
		encrypted: self encrypted;
		enabled: self enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		maxLength: self presenter maxLength;
		yourself.

	self presenter whenTextChangedDo: [ :text | newWidget setText: text ].
	self presenter whenPlaceholderChangedDo: [ :text | newWidget ghostText: text ].
	self presenter whenPasswordChangedDo: [ :isPassword | newWidget encrypted: isPassword ].
	self presenter whenMaxLengthChangedDo: [ :length | newWidget maxLength: length ].

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
