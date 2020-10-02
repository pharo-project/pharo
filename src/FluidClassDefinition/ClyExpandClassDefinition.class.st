Class {
	#name : #ClyExpandClassDefinition,
	#superclass : #SycSingleClassCommand,
	#category : #'FluidClassDefinition-Editor'
}

{ #category : #activation }
ClyExpandClassDefinition class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected 
]

{ #category : #activation }
ClyExpandClassDefinition class >> sourceCodeMenuActivation [
   <classAnnotation>
   ^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyClassDefinitionContext
]

{ #category : #accessing }
ClyExpandClassDefinition >> applyResultInContext: aToolContext [

	super applyResultInContext: aToolContext.
	aToolContext tool pendingText: targetClass definitionExpanded
]

{ #category : #accessing }
ClyExpandClassDefinition >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : #accessing }
ClyExpandClassDefinition >> defaultMenuItemName [
	^'Expand'
]

{ #category : #accessing }
ClyExpandClassDefinition >> execute [
	"
	empty on purpose logic is in applyResultInContext:
	"
]
