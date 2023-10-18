Class {
	#name : 'CDNilSubclassParserTest',
	#superclass : 'CDNormalClassParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDNilSubclassParserTest >> classDefinitionString [

	^ 'ProtoObject subclass: #ProtoObject
		slots: {slots};
	classVariableNames: ''firstClass secondClass''
	package: #MyPackage.
ProtoObject superclass: nil' format: { 'slots' -> self slotsArray } asDictionary
]

{ #category : 'helpers' }
CDNilSubclassParserTest >> className [
	^ 'ProtoObject'
]

{ #category : 'helpers' }
CDNilSubclassParserTest >> superclassName [

	^ 'nil'
]

{ #category : 'tests' }
CDNilSubclassParserTest >> testBestNodeForClassNameSelectionShouldBeClassNameNode [

	| selectedNode selection |
	selection := self selectionOf: 'ProtoObject'.
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition
]
