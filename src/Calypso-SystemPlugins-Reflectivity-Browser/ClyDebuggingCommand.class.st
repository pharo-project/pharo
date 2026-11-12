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
	
	aBrowserContext hasUnacceptedEdits ifTrue: [ ^ false ].
	
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
