"
I am a base class for commands which add/remove watchpoints, metalinks, breakpoints, and counters into given method or source node
"
Class {
	#name : 'ClyDebuggingCommand',
	#superclass : 'SycSourceCodeCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Commands',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyDebuggingCommand class >> canBeExecutedInContext: aBrowserContext [

	| node |
	^ self isAbstract not and: [
		  node := aBrowserContext lastSelectedSourceNode.

		  node isNotNil and: [
			  node isCommentNode not and: [
				  node isPragma not and: [
					  node isSequence not and: [
						  node isLiteralNode not and: [
							  (node isVariable and: [ node isDefinition ]) not ] ] ] ] ] ]
]

{ #category : 'activation' }
ClyDebuggingCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	self subclassResponsibility
]

{ #category : 'testing' }
ClyDebuggingCommand class >> isAbstract [
	^self = ClyDebuggingCommand
]

{ #category : 'activation' }
ClyDebuggingCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^SycDebuggingMenuActivation
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
ClyDebuggingCommand class >> methodEditorLeftBarMenuActivation [
	<classAnnotation>

	^CmdTextLeftBarMenuActivation
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]

{ #category : 'activation' }
ClyDebuggingCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycDebuggingMenuActivation
		byItemOf: ClyDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]
