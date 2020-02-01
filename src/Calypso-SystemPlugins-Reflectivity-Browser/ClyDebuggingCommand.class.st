"
I am a base class for commands which add/remove metalinks into given method or source node
"
Class {
	#name : #ClyDebuggingCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #activation }
ClyDebuggingCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	self subclassResponsibility
]

{ #category : #testing }
ClyDebuggingCommand class >> isAbstract [
	^self = ClyDebuggingCommand 
]

{ #category : #activation }
ClyDebuggingCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyDebuggingCommand class >> methodEditorLeftBarMenuActivation [
	<classAnnotation>
	
	^CmdTextLeftBarMenuActivation 
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]

{ #category : #activation }
ClyDebuggingCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycDebuggingMenuActivation 
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]
