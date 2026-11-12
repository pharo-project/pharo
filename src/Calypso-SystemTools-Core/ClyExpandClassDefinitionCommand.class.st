"
Expands the fluid definition to include all possible elements and shows the default values
"
Class {
	#name : 'ClyExpandClassDefinitionCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'Calypso-SystemTools-Core-Commands-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Commands-Classes'
}

{ #category : 'execution' }
ClyExpandClassDefinitionCommand >> applyCommandResult [

	super applyCommandResult.
	self context tool pendingText: self context tool editingClass expandedDefinitionString
]

{ #category : 'testing' }
ClyExpandClassDefinitionCommand >> canBeExecuted [

	^ true
]

{ #category : 'execution' }
ClyExpandClassDefinitionCommand >> executeRefactoring [
	"
	empty on purpose logic is in applyResultInContext:
	"
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> iconName [
	^#browse
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> name [

	^ 'Expand class definition'
]
