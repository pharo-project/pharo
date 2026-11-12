Class {
	#name : 'ClyExpandClassDefinitionCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'Calypso-SystemTools-Core-Commands-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Commands-Classes'
}

{ #category : 'testing' }
ClyExpandClassDefinitionCommand class >> canBeExecutedInContext: aToolContext [

	^ true 
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> applyCommandResult [

	super applyCommandResult.
	self context tool pendingText: self context tool editingClass expandedDefinitionString
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> description [

	^ 'Expands the fluid definition to include all possible elements and shows the default values'
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
