Class {
	#name : #CmdCommandExampleInRootMenu,
	#superclass : #CmdRootOfCommandExamples,
	#category : #'Commander-Core-Tests'
}

{ #category : #activation }
CmdCommandExampleInRootMenu class >> rootMenuActivationExample [
	<classAnnotation>
	
	^CmdMenuCommandActivationExample byRootGroupItemOrder: 1 for: CmdToolContextStub1
]
