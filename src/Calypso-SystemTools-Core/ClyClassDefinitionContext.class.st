"
I am the context of the class definition editor (ClyClassDefinitionEditorToolMorph): its source code is a class definition, not a method.
I answer false to the questions about variables and breakpoints that the method source code context answers from the selected node, so the commands of methods are not shown in the definition editor. Commands declared on me only apply to class definitions.
"
Class {
	#name : 'ClyClassDefinitionContext',
	#superclass : 'ClySourceCodeContext',
	#category : 'Calypso-SystemTools-Core-Editors-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Classes'
}

{ #category : 'commands' }
ClyClassDefinitionContext class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyExpandClassDefinitionCommand order: -20.
	(aCommandMap contextMenu group: SycSourceCodeMenuGroup) add: ClyExpandClassDefinitionCommand order: -100.
	aCommandMap shortcuts add: ClyExpandClassDefinitionCommand key: $e meta
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isArgOrTempVariableSelected [

	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isAssignmentSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isClassDefinitionContext [

	^ true
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isGlobalVariableSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isSelectedItemHasBreakpoint [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isTempVariableSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isVariableSelected [
	^ false
]
