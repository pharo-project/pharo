Class {
	#name : #SpMorphicCodeAdapter,
	#superclass : #SpMorphicTextAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicCodeAdapter >> behavior [

	^ self model behavior
]

{ #category : #factory }
SpMorphicCodeAdapter >> buildWidget [
	| newWidget |
	
	newWidget := super buildWidget.
	self presenter whenSyntaxHighlightChangedDo: [ :hasSyntaxHighlight | 
		self setEditingModeFor: newWidget ].
	
	^ newWidget
]

{ #category : #'private-shout' }
SpMorphicCodeAdapter >> classOrMetaClass: aClass [

	self 
		setEditingModeFor: self widget 
		withBehavior: aClass
]

{ #category : #'widget API' }
SpMorphicCodeAdapter >> doItContext [

	^ self model doItContext
]

{ #category : #'widget API' }
SpMorphicCodeAdapter >> doItReceiver [

	^ self model doItReceiver
]

{ #category : #NOCompletion }
SpMorphicCodeAdapter >> guessTypeForName: aString [

	^nil
]

{ #category : #private }
SpMorphicCodeAdapter >> hasSyntaxHighlight [
	^ self model hasSyntaxHighlight
]

{ #category : #NOCompletion }
SpMorphicCodeAdapter >> receiverClass [

	^ self behavior
]

{ #category : #'widget API' }
SpMorphicCodeAdapter >> selectedBehavior [

	^ self model selectedBehavior
]

{ #category : #NOCompletion }
SpMorphicCodeAdapter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #private }
SpMorphicCodeAdapter >> setEditingModeFor: textArea [

	self hasSyntaxHighlight
		ifTrue: [ self setEditingModeFor: textArea withBehavior: self behavior ]
		ifFalse: [ super setEditingModeFor: textArea ]
]

{ #category : #private }
SpMorphicCodeAdapter >> setEditingModeFor: textArea withBehavior: aBehavior [

	aBehavior
		ifNotNil: [ textArea beForSmalltalkCodeInClass: aBehavior ]		
		ifNil: [ textArea beForSmalltalkScripting ]
]

{ #category : #private }
SpMorphicCodeAdapter >> textWithStyle [
	"This method is to be used for testing. 
	 DO NOT USE IT!"
	
	^ self widget getText
]
