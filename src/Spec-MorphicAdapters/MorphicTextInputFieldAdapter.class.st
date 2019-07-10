"
I am the adapter used to bridge a TextInputFieldPresenter and a PluggableTextFieldMorph
"
Class {
	#name : #MorphicTextInputFieldAdapter,
	#superclass : #MorphicTextAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> accept: aText [

	self model accept: aText
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> accept: aText notifying: aNotifyier [
	"We use #asString because rubric send Text instead of strings."

	super accept: aText asString notifying: aNotifyier
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> acceptOnCR [

	^ self model acceptOnCR
]

{ #category : #initialization }
MorphicTextInputFieldAdapter >> adapt: aModel [

	super adapt: aModel.
	aModel whenBuiltDo: [ :w | w widget color: Smalltalk ui theme backgroundColor ]
]

{ #category : #factory }
MorphicTextInputFieldAdapter >> buildWidget [
	| newWidget |
	self flag: #pharo7.	"When support for P7 will be dropped, remove this compatibility hack."
	newWidget := ((self class environment at: #SpecRubTextFieldMorph ifAbsent: [ RubTextFieldMorph ]) on: self)
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
MorphicTextInputFieldAdapter >> encrypted [
	^ self model isPassword
]

{ #category : #'spec protocol' }
MorphicTextInputFieldAdapter >> encrypted: aBoolean [

	self widgetDo: [ :w | 
		w encrypted: aBoolean.
		w changed ]
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> entryCompletion [

	^ self model entryCompletion
]

{ #category : #testing }
MorphicTextInputFieldAdapter >> isPassword [
	
	^ self widget font isKindOf: FixedFaceFont
]

{ #category : #accessing }
MorphicTextInputFieldAdapter >> placeHolderText [
	
	^ widget ghostText 
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> placeholder [
	^ self model placeholder
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> placeholder: aText [
	^ self model placeholder: aText
]

{ #category : #accessing }
MorphicTextInputFieldAdapter >> text: aString [
	widget
		setText: aString;
		acceptTextInModel
]
