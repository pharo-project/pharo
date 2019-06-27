"
I am context of source code editor tool.
I am based on the ast of editing code.

I provide following interface to query code editor state. It is polymorphic to system browser context:

- selectedClasses 
- lastSeletedClass 
- isClassSelected 

- selectedMethods
- lastSelectedMethod 
- isMethodSelected 

- selectedMessages
- lastSelectedMessage 
- isMessageSelected 

- selectedVariables
- lastSelectedVariable
- isVariableSelected
- isTempVariableSelected 

Internal Representation and Key Implementation Points.

    Instance Variables
	selectedSourceNode:		<RBProgramNode>
"
Class {
	#name : #ClySourceCodeContext,
	#superclass : #ClyTextEditorContext,
	#instVars : [
		'selectedSourceNode'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #'instance creation' }
ClySourceCodeContext class >> for: anEditorTool selectedNode: aProgramNode [

	^(self for: anEditorTool)
		selectedSourceNode: aProgramNode
]

{ #category : #'user requests' }
ClySourceCodeContext >> confirmUnusedClasses: variables [
	^self browser confirmUnusedClasses: variables
]

{ #category : #'user requests' }
ClySourceCodeContext >> confirmUnusedVariables: variables [
	^self browser confirmUnusedVariables: variables
]

{ #category : #'user requests' }
ClySourceCodeContext >> confirmUnusedVariablesInDefiningClass: variables [
	^self browser confirmUnusedVariablesInDefiningClass: variables
]

{ #category : #'command execution' }
ClySourceCodeContext >> executeCommand: aCommand by: aCommandActivator [

	aCommand isComplexRefactoring ifTrue: [
		aCommand confirmRefactoringInContext: self by: aCommandActivator].
	
	aCommand execute 
]

{ #category : #testing }
ClySourceCodeContext >> isAssignmentSelected [
	| node |
	node := self selectedSourceNode.
	[node notNil] whileTrue: [ 
		node isAssignment ifTrue: [ ^true ].
		node := node parent].
	^false
]

{ #category : #testing }
ClySourceCodeContext >> isClassSelected [

	| node |
	node := self selectedSourceNode.
	node isVariable ifFalse: [ ^false ].
	
	^node binding isLiteralVariable 
		and: [ node binding isGlobalClassNameBinding ]
]

{ #category : #testing }
ClySourceCodeContext >> isGlobalVariableSelected [
	| node binding |
	node := self selectedSourceNode.
	node isVariable ifFalse: [ ^false ].
	
	binding := node binding.
	
	^binding isGlobalVariable
]

{ #category : #testing }
ClySourceCodeContext >> isMessageSelected [

	^self selectedSourceNode isMessage
]

{ #category : #testing }
ClySourceCodeContext >> isMethodSelected [ 
	^false
]

{ #category : #testing }
ClySourceCodeContext >> isTempVariableSelected [
	| node |
	node := self selectedSourceNode.
	^node isVariable and: [ node isTemp]
]

{ #category : #testing }
ClySourceCodeContext >> isVariableSelected [
	| node binding |
	self isClassSelected ifTrue: [ ^false ].
	
	node := self selectedSourceNode.
	node isVariable ifFalse: [ ^false ].
	
	binding := node binding.
	
	^binding isClassVariable | binding isInstance | binding isGlobalVariable | binding isUndeclared
]

{ #category : #selection }
ClySourceCodeContext >> lastSelectedClass [

	^self selectedClass
]

{ #category : #selection }
ClySourceCodeContext >> lastSelectedMessage [

	^self selectedMessage
]

{ #category : #selection }
ClySourceCodeContext >> lastSelectedSourceNode [

	^self selectedSourceNode
]

{ #category : #selection }
ClySourceCodeContext >> lastSelectedVariable [

	^self selectedVariable 
]

{ #category : #'refactoring support' }
ClySourceCodeContext >> refactoringScopes [
	^self browser allNavigationScopes
]

{ #category : #selection }
ClySourceCodeContext >> selectedClass [

	^self selectedSourceNode binding value
]

{ #category : #selection }
ClySourceCodeContext >> selectedClasses [

	^{self selectedClass}
]

{ #category : #selection }
ClySourceCodeContext >> selectedMessage [
	^SycMessageDescription ofMessageNode: self selectedSourceNode
]

{ #category : #selection }
ClySourceCodeContext >> selectedMessages [

	^{self selectedMessage}
]

{ #category : #accessing }
ClySourceCodeContext >> selectedSourceNode [
	^selectedSourceNode
]

{ #category : #accessing }
ClySourceCodeContext >> selectedSourceNode: anObject [
	selectedSourceNode := anObject
]

{ #category : #selection }
ClySourceCodeContext >> selectedVariable [

	^self selectedSourceNode binding asCalypsoVariableOf: tool editingClass
]

{ #category : #selection }
ClySourceCodeContext >> selectedVariables [

	^{self selectedVariable}
]

{ #category : #'tool controlling' }
ClySourceCodeContext >> showSourceNode [

	tool selectSourceNode: selectedSourceNode
]

{ #category : #'tool controlling' }
ClySourceCodeContext >> showVariableNamed: aString [
	tool selectVariableNamed: aString
]
