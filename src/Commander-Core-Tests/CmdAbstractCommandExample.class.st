Class {
	#name : #CmdAbstractCommandExample,
	#superclass : #CmdRootOfCommandExamples,
	#category : #'Commander-Core-Tests'
}

{ #category : #activation }
CmdAbstractCommandExample class >> activationExample1 [
	<classAnnotation>
	
	^CmdCommandActivationExample for: CmdToolContextStub1
]

{ #category : #testing }
CmdAbstractCommandExample class >> isAbstract [
	^self = CmdAbstractCommandExample 
]
