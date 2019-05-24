Class {
	#name : #CmdCommandExampleInDeepMenuGroup,
	#superclass : #CmdRootOfCommandExamples,
	#category : #'Commander-Core-Tests'
}

{ #category : #activation }
CmdCommandExampleInDeepMenuGroup class >> menuActivationExample [
	<classAnnotation>
	
	^CmdMenuCommandActivationExample byItemOf: CmdDeepChildMenuGroupStub for: CmdToolContextStub2
]
