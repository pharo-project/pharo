Class {
	#name : #ClyExpandClassDefinitionCommand,
	#superclass : #SycSingleClassCommand,
	#category : #'Calypso-SystemTools-Core-Commands-Classes'
}

{ #category : #activation }
ClyExpandClassDefinitionCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected 
]

{ #category : #activation }
ClyExpandClassDefinitionCommand class >> sourceCodeMenuActivation [
   <classAnnotation>
   ^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyClassDefinitionContext
]

{ #category : #accessing }
ClyExpandClassDefinitionCommand >> applyResultInContext: aToolContext [

	super applyResultInContext: aToolContext.
	aToolContext tool pendingText: targetClass expandedDefinitionString
]

{ #category : #accessing }
ClyExpandClassDefinitionCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : #accessing }
ClyExpandClassDefinitionCommand >> defaultMenuItemName [
	^'Expand'
]

{ #category : #accessing }
ClyExpandClassDefinitionCommand >> execute [
	"
	empty on purpose logic is in applyResultInContext:
	"
]
