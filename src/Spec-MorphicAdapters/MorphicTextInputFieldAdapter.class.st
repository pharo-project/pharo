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
	| plu |
	plu := PluggableTextFieldMorph new
		convertTo: String;
		on: self
			text: #getText
			accept: #accept:
			readSelection: nil
			menu: nil
			setSelection: #setSelection:.
	plu
		entryCompletion: self entryCompletion;
		autoAccept: self autoAccept;
		ghostText: self placeholder;
		enabled: self enabled;
		encrypted: self encrypted;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		acceptOnCR: self acceptOnCR;
		hideScrollBarsIndefinitely.
	^ plu
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> encrypted [

	^ self model encrypted
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

{ #category : #NOCompletion }
MorphicTextInputFieldAdapter >> isCodeCompletionAllowed [
	
	^ false
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> placeholder [
	^ self model placeholder
]

{ #category : #'widget API' }
MorphicTextInputFieldAdapter >> placeholder: aText [
	^ self model placeholder: aText
]
