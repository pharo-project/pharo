Class {
	#name : 'CDNilSubclassParserTest',
	#superclass : 'CDNormalClassParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDNilSubclassParserTest >> classDefinitionString [

	^ 'nil << #ProtoObject
		slots: {slots};
		sharedVariables: {sharedVariables};
		package: #MyPackage' format: {
			  'slots' -> self slotsArray.
			  'sharedVariables' -> self sharedVariablesArray } asDictionary
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
	self assert: selectedNode equals: classDefinition classNameNode
]
