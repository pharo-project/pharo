Class {
	#name : #CmdCommandExampleInMultipleMenuGroups,
	#superclass : #CmdRootOfCommandExamples,
	#category : #'Commander-Core-Tests'
}

{ #category : #activation }
CmdCommandExampleInMultipleMenuGroups class >> childMenuGroupActivationExample1 [
	<classAnnotation>
	
	^CmdMenuCommandActivationExample byItemOf: CmdMenuGroupExample1 for: CmdToolContextStub1
]

{ #category : #activation }
CmdCommandExampleInMultipleMenuGroups class >> childMenuGroupActivationExample2 [
	<classAnnotation>
	 
	^CmdMenuCommandActivationExample byItemOf: CmdMenuGroupExample2 for: CmdToolContextStub1
]

{ #category : #activation }
CmdCommandExampleInMultipleMenuGroups class >> rootMenuActivationExample [
	<classAnnotation>
	
	^CmdMenuCommandActivationExample byRootGroupItemOrder: 2 for: CmdToolContextStub1
]
