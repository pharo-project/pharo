Class {
	#name : #CodeParserTest,
	#superclass : #TestCase,
	#category : #'ClassParser-Tests'
}

{ #category : #tests }
CodeParserTest >> testArbitraryExpressionStringReturnsProgramNodeThatIsNotMethodNode [

	| astNode |
	astNode := CodeParser parse: '1 + ( self foo: #( asd gdf 434 4.0 ))'.
	self assert: astNode isMessage
]

{ #category : #tests }
CodeParserTest >> testClassDefinitionStringReturnsClassDefinitionNode [

	| astNode |
	astNode := CodeParser parse: 'Box subclass: #Box2
		instanceVariableNames: ''''
		classVariableNames:''''
		package: ''Box'''.
	self assert: astNode isClassDefinition
]

{ #category : #tests }
CodeParserTest >> testMethodDefinitionStringReturnsMethodNode [

	| astNode |
	astNode := CodeParser parse: 'm1
	^ self foo'.
	self assert: astNode isMethod
]
