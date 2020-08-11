Class {
	#name : #SindarinStepBytecodeCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinStepBytecodeCommand class >> defaultDescription [
	^ 'Step a single bytecode'
]

{ #category : #initialization }
SindarinStepBytecodeCommand class >> defaultIconName [
	^#glamorousInto 
]

{ #category : #default }
SindarinStepBytecodeCommand class >> defaultName [
	^ 'Step Bytecode'
]

{ #category : #hooks }
SindarinStepBytecodeCommand >> execute [
	self context stepBytecode
]
