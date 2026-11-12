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
