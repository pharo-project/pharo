Class {
	#name : #RBAddVariableAccessorTransformationTest1,
	#superclass : #RBParametrizedTransformationTest,
	#instVars : [
		'targetClass',
		'var',
		'isClassSide',
		'hasChanges',
		'methods'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddVariableAccessorTransformationTest1 class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #targetClass -> #RBDummyLintRuleTest. 
						#var -> 'name'. 
						#isClassSide -> false.
						#hasChanges -> false.
						#methods -> { #name -> 'name ^name' . 
							#name: -> 'name: aString name := aString' } };
		addCase: { #targetClass -> #RBDummyLintRuleTest. 
						#var -> 'Foo1'. 
						#isClassSide -> true.
						#hasChanges -> true.
						#methods -> { #foo1 -> 'foo1 ^Foo1' . 
							#foo1: -> 'foo1: anObject Foo1 := anObject' } };
		addCase: { #targetClass -> #RBDummyLintRuleTest. 
						#var -> 'foo1'. 
						#isClassSide -> false.
						#hasChanges -> true.
						#methods -> { #foo1 -> 'foo1 ^foo1' . 
							#foo1: -> 'foo1: anObject foo1 := anObject' } };
		addCase: { #targetClass -> #RBLintRuleTestData. 
						#var -> 'name'. 
						#isClassSide -> false.
						#hasChanges -> false.
						#methods -> { #name -> 'name ^name' . 
							#name: -> 'name: aString name := aString' } };
		addCase: { #targetClass -> #RBLintRuleTestData. 
						#var -> 'Name1'. 
						#isClassSide -> true.
						#hasChanges -> false.
						#methods -> { #name1 -> 'name1 ^Name1' . 
							#name1: -> 'name1: anObject Name1 := anObject' } };
		yourself
]

{ #category : #'instance creation' }
RBAddVariableAccessorTransformationTest1 >> createInstance [ 
	^ isClassSide 
		ifFalse: [ RBAddVariableAccessorTransformation instanceVariable: var class: targetClass ]
		ifTrue: [ RBAddVariableAccessorTransformation classVariable: var class: targetClass ]
]

{ #category : #'accessing - model' }
RBAddVariableAccessorTransformationTest1 >> getRBClassOf: aRefactoring [
	^ isClassSide 
	ifTrue: [ aRefactoring model metaclassNamed: targetClass ]
	ifFalse: [ aRefactoring model classNamed: targetClass ]
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> hasChanges [

	^ hasChanges
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> hasChanges: anObject [

	hasChanges := anObject
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> isClassSide [

	^ isClassSide
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> isClassSide: anObject [

	isClassSide := anObject
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> methods [

	^ methods
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> methods: anObject [

	methods := anObject
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> targetClass [

	^ targetClass
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> targetClass: anObject [

	targetClass := anObject
]

{ #category : #'failure tests' }
RBAddVariableAccessorTransformationTest1 >> testNonExistantName [
	self
		shouldFail: (RBAddVariableAccessorTransformation 
						classVariable: #Foo
						class: #RBBasicLintRuleTestData)
						asRefactoring;
		shouldFail: (RBAddVariableAccessorTransformation 
						instanceVariable: 'foo'
						class: #RBBasicLintRuleTestData)
						asRefactoring
]

{ #category : #tests }
RBAddVariableAccessorTransformationTest1 >> testRefactoring [ 
	| refactoring rbClass |
	refactoring := self createInstance asRefactoring transform.
	rbClass := self getRBClassOf: refactoring.
	hasChanges ifTrue: [ self denyEmpty: refactoring changes changes ]
		ifFalse: [ self assertEmpty: refactoring changes changes ].
	self assert: (rbClass parseTreeFor: methods first key)
		equals: (self parseMethod: methods first value).
	self assert: (rbClass parseTreeFor: methods second key) 
		equals: (self parseMethod: methods second value)	
]

{ #category : #tests }
RBAddVariableAccessorTransformationTest1 >> testTransform [
	| transformation rbClass |
	transformation := self createInstance transform.
	rbClass := self getRBClassOf: transformation.
	hasChanges 
		ifTrue: [ self denyEmpty: transformation changes changes ]
		ifFalse: [ self assertEmpty: transformation changes changes ].
	self assert: (rbClass parseTreeFor: methods first key)
		equals: (self parseMethod: methods first value).
	self assert: (rbClass parseTreeFor: methods second key) 
		equals: (self parseMethod: methods second value)	
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> var [

	^ var
]

{ #category : #accessing }
RBAddVariableAccessorTransformationTest1 >> var: anObject [

	var := anObject
]
