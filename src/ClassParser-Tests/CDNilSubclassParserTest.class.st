Class {
	#name : #CDNilSubclassParserTest,
	#superclass : #CDNormalClassParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDNilSubclassParserTest >> classDefinitionString [
	^ 'ProtoObject subclass: #ProtoObject
	instanceVariableNames: ''first second''
	classVariableNames: ''firstClass secondClass''
	package: #MyPackage.
ProtoObject superclass: nil'
]

{ #category : #helpers }
CDNilSubclassParserTest >> className [
	^ 'ProtoObject'
]

{ #category : #helpers }
CDNilSubclassParserTest >> superclassName [

	^ 'nil'
]

{ #category : #tests }
CDNilSubclassParserTest >> testBestNodeForClassNameSelectionShouldBeClassNameNode [

	| selectedNode selection |
	selection := self selectionOf: 'ProtoObject'.
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition
]
