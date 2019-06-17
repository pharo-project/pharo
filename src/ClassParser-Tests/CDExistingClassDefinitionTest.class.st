Class {
	#name : #CDExistingClassDefinitionTest,
	#superclass : #CDBehaviorParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDExistingClassDefinitionTest >> classDefinitionString [

	^ CDClassWithFullDefinitionExample definition 
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> className [

	^ CDClassWithFullDefinitionExample name
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> firstInstanceVariableName [
	
	^ 'instVar1' 
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> secondInstanceVariableName [

	^ 'instVar2'
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testClassNameNodeIsPolymorphicToRBVariableNode [
	
	| nameNode |
	nameNode := classDefinition classNameNode.
	self assert: nameNode isVariable. "It is polymorphic to class reference nodes in method sources"
	self deny: nameNode isTemp
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testGettingExistingClass [
	
	| class |
	class := classDefinition existingClassIfAbsent: [].
	self assert: class equals: CDClassWithFullDefinitionExample 
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testGettingExistingClassNameBinding [
	
	| binding |
	binding := classDefinition classNameNode binding.
	
	self assert: binding class equals: OCLiteralVariable. "to be compatible to bindings in RB nodes".
	self assert: binding assoc equals: CDClassWithFullDefinitionExample binding
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testSharedSlotNodeArePolymorphicToRBVariableNodes [
	
	| classVarNode |
	classVarNode := classDefinition sharedSlotNodes first.
	
	self assert: classVarNode isVariable.
	self deny: classVarNode isTemp.
	self deny: classVarNode isGlobalVariable.
	self assert: classVarNode isClassVariable.
	self deny: classVarNode isInstance.
	self deny: classVarNode isLiteralVariable.
	self deny: classVarNode isUndeclared.
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testSlotNodeArePolymorphicToRBVariableNodes [
	
	| slotNode |
	slotNode := classDefinition slotNodes first.
	self assert: slotNode isVariable.
	self deny: slotNode isTemp.
	self deny: slotNode isGlobalVariable.
	self deny: slotNode isClassVariable.
	self assert: slotNode isInstance.
	self deny: slotNode isLiteralVariable.
	self deny: slotNode isUndeclared.
]

{ #category : #helpers }
CDExistingClassDefinitionTest >> testSlotNodeBinding [
	
	| slotNode |
	slotNode := classDefinition slotNodes first.
	self assert: slotNode binding identicalTo: slotNode
]
