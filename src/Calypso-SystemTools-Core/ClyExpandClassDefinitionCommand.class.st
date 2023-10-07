Class {
	#name : 'ClyExpandClassDefinitionCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'Calypso-SystemTools-Core-Commands-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Commands-Classes'
}

{ #category : 'activation' }
ClyExpandClassDefinitionCommand class >> sourceCodeMenuActivation [
   <classAnnotation>
   ^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyClassDefinitionContext
]

{ #category : 'activation' }
ClyExpandClassDefinitionCommand class >> sourceCodeShortcutActivation [
   <classAnnotation>

   ^CmdShortcutActivation by: $e meta for: ClyClassDefinitionContext
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> applyResultInContext: aToolContext [

	super applyResultInContext: aToolContext.
	aToolContext tool pendingText: aToolContext tool editingClass expandedDefinitionString
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> defaultMenuItemName [
	^'Expand'
]

{ #category : 'accessing' }
ClyExpandClassDefinitionCommand >> execute [
	"
	empty on purpose logic is in applyResultInContext:
	"
]
