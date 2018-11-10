"
I am a base class for commands which add/remove metalinks into given method or source node
"
Class {
	#name : #ClyMetalinkCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #activation }
ClyMetalinkCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	self subclassResponsibility
]

{ #category : #activation }
ClyMetalinkCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMetalinkMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyMetalinkCommand class >> methodEditorLeftBarMenuActivation [
	<classAnnotation>
	
	^CmdTextLeftBarMenuActivation 
		byItemOf: ClyMetalinkMenuGroup order: self contextMenuOrder for: ClySourceCodeContext
]

{ #category : #activation }
ClyMetalinkCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation 
		byItemOf: ClyMetalinkMenuGroup order: self contextMenuOrder for: ClySourceCodeContext
]
