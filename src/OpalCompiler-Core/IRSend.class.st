"
Instruction ""send: selector"" or ""send: selector toSuperOf: behavior""
"
Class {
	#name : 'IRSend',
	#superclass : 'IRInstruction',
	#instVars : [
		'selector',
		'superOf'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRSend >> accept: arg1 [

	^ arg1 visitSend: self
]

{ #category : 'testing' }
IRSend >> isSend [

	^ true
]

{ #category : 'testing' }
IRSend >> isSuperSend [

	^ superOf isNotNil
]

{ #category : 'accessing' }
IRSend >> selector [

	^ selector
]

{ #category : 'accessing' }
IRSend >> selector: arg1 [

	selector := arg1
]

{ #category : 'accessing' }
IRSend >> superOf [

	^ superOf
]

{ #category : 'accessing' }
IRSend >> superOf: arg1 [

	superOf := arg1
]
