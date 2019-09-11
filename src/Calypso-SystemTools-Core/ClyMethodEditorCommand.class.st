"
I a base class for commands which manages the method editor.

Subclasses can be executed in context of ClyMetodEditor.

Internal Representation and Key Implementation Points.

    Instance Variables
	methodEditor:		<ClyMethodEditor>
"
Class {
	#name : #ClyMethodEditorCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'methodEditor'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #testing }
ClyMethodEditorCommand class >> canBeExecutedInClassEditor: aTool [

	^ false
]

{ #category : #testing }
ClyMethodEditorCommand class >> canBeExecutedInCodeMethodEditor: aTool [
	
	^ true
]

{ #category : #testing }
ClyMethodEditorCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext tool isCommandAvailable: self
]

{ #category : #testing }
ClyMethodEditorCommand class >> canBeExecutedInMethodEditor: aTool [

	^ false
]

{ #category : #testing }
ClyMethodEditorCommand class >> isAbstract [
	^self = ClyMethodEditorCommand 
]

{ #category : #activation }
ClyMethodEditorCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation for: ClyTextEditorContext
]

{ #category : #accessing }
ClyMethodEditorCommand >> methodEditor [
	^ methodEditor
]

{ #category : #accessing }
ClyMethodEditorCommand >> methodEditor: anObject [
	methodEditor := anObject
]

{ #category : #execution }
ClyMethodEditorCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	methodEditor := aToolContext tool
]
