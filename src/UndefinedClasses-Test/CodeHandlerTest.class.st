Class {
	#name : #CodeHandlerTest,
	#superclass : #TestCase,
	#category : #'UndefinedClasses-Test'
}

{ #category : #tests }
CodeHandlerTest >> testArbitraryExpressionStringReturnsProgramNodeThatIsNotMethodNode [

	| astNode |
	astNode := CodeParser parse: '1 + ( self foo: #( asd gdf 434 4.0 ))'.
	self assert: astNode isMessage
]

{ #category : #tests }
CodeHandlerTest >> testClassDefinitionStringReturnsClassDefinitionNode [

	| astNode |
	astNode := CodeParser parse: 'Box subclass: #Box2
		instanceVariableNames: ''''
		classVariableNames:''''
		package: ''Box'''.
	self assert: astNode isClassDefinition
]

{ #category : #tests }
CodeHandlerTest >> testMethodDefinitionStringReturnsMethodNode [

	| astNode |
	astNode := CodeParser parse: 'm1
	^ self foo'.
	self assert: astNode isMethod
]
