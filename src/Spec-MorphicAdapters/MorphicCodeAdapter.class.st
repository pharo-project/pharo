Class {
	#name : #MorphicCodeAdapter,
	#superclass : #MorphicTextAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'widget API' }
MorphicCodeAdapter >> behavior [

	^ self model behavior
]

{ #category : #factory }
MorphicCodeAdapter >> buildWidget [
	| newWidget |
	newWidget := super buildWidget.

	self presenter whenSyntaxHighlightChangedDo: [ :hasSyntaxHighlight | self setEditingModeFor: newWidget ].
	
	^ newWidget
]

{ #category : #'private-shout' }
MorphicCodeAdapter >> classOrMetaClass: aClass [

	self 
		setEditingModeFor: self widget 
		withBehavior: aClass
]

{ #category : #'widget API' }
MorphicCodeAdapter >> doItContext [

	^ self model doItContext
]

{ #category : #'widget API' }
MorphicCodeAdapter >> doItReceiver [

	^ self model doItReceiver
]

{ #category : #NOCompletion }
MorphicCodeAdapter >> guessTypeForName: aString [

	^nil
]

{ #category : #private }
MorphicCodeAdapter >> hasSyntaxHighlight [
	^ self model hasSyntaxHighlight
]

{ #category : #NOCompletion }
MorphicCodeAdapter >> isCodeCompletionAllowed [

	^ true
]

{ #category : #NOCompletion }
MorphicCodeAdapter >> receiverClass [

	^ self behavior
]

{ #category : #'widget API' }
MorphicCodeAdapter >> selectedBehavior [

	^ self model selectedBehavior
]

{ #category : #NOCompletion }
MorphicCodeAdapter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #private }
MorphicCodeAdapter >> setEditingModeFor: textArea [
	self hasSyntaxHighlight
		ifTrue: [ self setEditingModeFor: textArea withBehavior: self behavior ]
		ifFalse: [ super setEditingModeFor: textArea ]
]

{ #category : #private }
MorphicCodeAdapter >> setEditingModeFor: textArea withBehavior: aBehavior [

	aBehavior
		ifNotNil: [ textArea beForSmalltalkCodeInClass: aBehavior ]		
		ifNil: [ textArea beForSmalltalkScripting ]
]

{ #category : #private }
MorphicCodeAdapter >> textWithStyle [
	"This method is to be used for testing. 
	 DO NOT USE IT!"
	
	^ self widget getText
]
