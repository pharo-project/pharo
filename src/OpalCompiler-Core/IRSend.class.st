"
Instruction ""send: selector"" or ""send: selector toSuperOf: behavior""
"
Class {
	#name : #IRSend,
	#superclass : #IRInstruction,
	#instVars : [
		'selector',
		'superOf'
	],
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRSend >> accept: aVisitor [
	^ aVisitor visitSend: self
]

{ #category : #testing }
IRSend >> isSend [
	^true.
]

{ #category : #testing }
IRSend >> isSuperSend [
    ^superOf notNil
]

{ #category : #accessing }
IRSend >> selector [
	^selector
]

{ #category : #accessing }
IRSend >> selector: symbol [

	selector := symbol
]

{ #category : #accessing }
IRSend >> superOf [

	^ superOf 
]

{ #category : #accessing }
IRSend >> superOf: behavior [

	superOf := behavior
]
