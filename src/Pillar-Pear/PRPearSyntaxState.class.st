"
I am a state pattern to switch syntax between **MicroDown** and **Pillar** for now. We can think of adding GitHub Markdown.
"
Class {
	#name : #PRPearSyntaxState,
	#superclass : #Object,
	#category : #'Pillar-Pear'
}

{ #category : #accessing }
PRPearSyntaxState >> exampleText [
	^ self subclassResponsibility 
]

{ #category : #accessing }
PRPearSyntaxState >> label [
	^ 'Change to ', self next new name, ' syntax'.
]

{ #category : #accessing }
PRPearSyntaxState >> name [
	^ self subclassResponsibility 
]

{ #category : #accessing }
PRPearSyntaxState >> next [
	^ self subclassResponsibility 
]

{ #category : #parsing }
PRPearSyntaxState >> parse: aText [
	^ self parser new parse: aText asString
]

{ #category : #accessing }
PRPearSyntaxState >> parser [
	^ self subclassResponsibility 
]

{ #category : #writing }
PRPearSyntaxState >> write: aStringOrStream [
	| parsedAsPillar writtenText|
	parsedAsPillar := self parse: aStringOrStream.
	writtenText := self next writer new start: parsedAsPillar; contents.
	^ writtenText
]

{ #category : #accessing }
PRPearSyntaxState >> writer [
	^ self subclassResponsibility 
]
