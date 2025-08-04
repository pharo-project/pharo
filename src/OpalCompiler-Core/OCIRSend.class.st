"
Instruction ""send: selector"" or ""send: selector toSuperOf: behavior""
"
Class {
	#name : 'OCIRSend',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'selector',
		'superOf'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRSend >> accept: aVisitor [
	^ aVisitor visitSend: self
]

{ #category : 'testing' }
OCIRSend >> isSend [
	^true
]

{ #category : 'testing' }
OCIRSend >> isSuperSend [

    ^ superOf isNotNil
]

{ #category : 'accessing' }
OCIRSend >> selector [
	^selector
]

{ #category : 'accessing' }
OCIRSend >> selector: symbol [

	selector := symbol
]

{ #category : 'accessing' }
OCIRSend >> superOf [

	^ superOf
]

{ #category : 'accessing' }
OCIRSend >> superOf: behavior [

	superOf := behavior
]
