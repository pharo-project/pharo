Class {
	#name : #CDExistingClassSideDefinitionTest,
	#superclass : #CDExistingClassDefinitionTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDExistingClassSideDefinitionTest >> classDefinitionString [

	^ (ClassDefinitionPrinter oldPharo for: CDClassWithFullDefinitionExample class) definitionString
]

{ #category : #helpers }
CDExistingClassSideDefinitionTest >> firstInstanceVariableName [
	
	^ 'classSideVar1' 
]

{ #category : #helpers }
CDExistingClassSideDefinitionTest >> secondInstanceVariableName [

	^ 'classSideVar2'
]

{ #category : #helpers }
CDExistingClassSideDefinitionTest >> testGettingExistingClass [
	
	| class |
	class := classDefinition existingClassIfAbsent: [].
	self assert: class equals: CDClassWithFullDefinitionExample class
]
