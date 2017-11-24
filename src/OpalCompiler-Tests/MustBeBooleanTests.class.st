Class {
	#name : #MustBeBooleanTests,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
MustBeBooleanTests >> testAnd [
	| myBooleanObject |
	
	myBooleanObject := MyBooleanObject new.
	self deny: (myBooleanObject and: [true])
]

{ #category : #tests }
MustBeBooleanTests >> testDNU [
	| code |
	code := [ (true ifFalse:[]) ifTrue:[] ].
	self should: code raise: MessageNotUnderstood .
	code on: MessageNotUnderstood do: [ :error | self assert: error receiver isNil. 
			self assert: error message selector equals: #ifTrue:]
]

{ #category : #tests }
MustBeBooleanTests >> testIfTrueEffect [
	| temp fakeBool |
	
	fakeBool := MyBooleanObject new.
	temp := 1.
	fakeBool ifTrue: [ temp := 5 + 3 + 1 ].
	self assert: temp equals: 9.
	
	fakeBool ifTrue: [ [  ] ].
	"fakeBool ifTrue: [ 1+ 2. [ :a | a ] value: 5. 7 ]."
]

{ #category : #tests }
MustBeBooleanTests >> testIfTrueValue [
	| myBooleanObject |
	
	self assert: (MyBooleanObject new ifTrue: [ 1 + 2 ]) equals: '3 sent from my boolean object'.
	myBooleanObject := MyBooleanObject new.
	self assert: (myBooleanObject ifTrue: [ 1 + 2 ]) equals: '3 sent from my boolean object'.
]

{ #category : #tests }
MustBeBooleanTests >> testIfTrueWithClosureAfterJump [
	"A closure is a multibyte instruction, that should not be
	1) confused for an ending jump in ifTrue/ifFalse structure
	2) set as jumptarget, or have PC manually set to, none but its first byte."
	| myBooleanObject |
	myBooleanObject := MyBooleanObject new.
	myBooleanObject
		ifTrue: [ 4 + 3 ].
	[ :string | 
	self assert: 'a ' , string equals: 'a 5' ] value: '5'
]

{ #category : #tests }
MustBeBooleanTests >> testOr [
	| myBooleanObject |
	
	myBooleanObject := MyBooleanObject new.
	self assert: (myBooleanObject or: [true])
]

{ #category : #tests }
MustBeBooleanTests >> testWhile [
	| myFlag |
	self should: [[ nil ] whileFalse: [myFlag := true ]] raise: MessageNotUnderstood.
]
