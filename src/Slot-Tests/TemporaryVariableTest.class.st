"
Tests for TemporaryVariable 
"
Class {
	#name : 'TemporaryVariableTest',
	#superclass : 'TestCase',
	#category : 'Slot-Tests-VariablesAndSlots',
	#package : 'Slot-Tests',
	#tag : 'VariablesAndSlots'
}

{ #category : 'tests' }
TemporaryVariableTest >> testDefiningNode [
	"test for normal temps, copied and tempvector"
	| ast tempsNodes tempDefintions  tempUses |
	ast := self class compiler parse: 'myMethod: arg
 	 | test testCopied testTempVector|
 	 test := 2.
  	 test.
  	 [ testCopied. testTempVector := 1 ].
    ^test'.
	tempsNodes := ast allChildren select: [ :each | each isTempVariable ].
	tempDefintions := tempsNodes select:  [ :each | each isDefinition ].
	tempUses := tempsNodes reject:  [ :each | each isDefinition ].
	tempUses do: [ :use | self assert: (tempDefintions includes: use variable definingNode)].

]

{ #category : 'tests' }
TemporaryVariableTest >> testHasTemporaryVariablesBlock [
	| block |
	block := [ | b | b := 2. b +2  ].
	self assert: (block hasTemporaryVariableNamed: #b)
]

{ #category : 'tests' }
TemporaryVariableTest >> testHasTemporaryVariablesMethod [
	| method |
	method := self class >> #testTemporaryVariablesMethod.
	self assert: (method hasTemporaryVariableNamed: #method)
]

{ #category : 'tests' }
TemporaryVariableTest >> testIsReferenced [
	| method notReferenced |
	"The temp notReferenced is not used as we test exactly that here"
	method := self class >> #testIsReferenced.
	self assert: method temporaryVariables first isReferenced.
	self deny: method temporaryVariables second isReferenced
]

{ #category : 'properties' }
TemporaryVariableTest >> testPropertyAtPut [

	| testValue temp |

	testValue := Date today.
	temp := thisContext lookupVar:  #testValue.

	temp propertyAt: #testKeySelector put: testValue.
	self
		assert: (temp propertyAt: #testKeySelector)
		equals: testValue.

	temp removeProperty: #testKeySelector.
	self assert: temp properties isNil
]

{ #category : 'tests' }
TemporaryVariableTest >> testReadNodes [
	| method |
	"On the level of CompiledMethod, args include temps. This needs to be fixed"
	method := OrderedCollection >> #do:.
	self assert: (method lookupVar: #aBlock) astNodes notEmpty
]

{ #category : 'tests' }
TemporaryVariableTest >> testReadTemporaryVariablesMethod [
	| tempVar |
	tempVar := thisContext lookupVar: #tempVar.

	self assert: (tempVar readInContext: thisContext) equals: tempVar
]

{ #category : 'tests' }
TemporaryVariableTest >> testScope [
	| method variable |
	method := self class >> #testTemporaryVariablesMethod.
	self assert: method temporaryVariables first scope isMethodScope.

	variable := method temporaryVariables first.
	self assert: (variable scope lookupVar: variable name) equals: variable
]

{ #category : 'tests' }
TemporaryVariableTest >> testTemporaryVariablesBlock [
	| block |
	block := [ | b | b +2  ].
	self assert: block temporaryVariables first name equals: #b
]

{ #category : 'tests' }
TemporaryVariableTest >> testTemporaryVariablesMethod [
	| method |
	method := self class >> #testTemporaryVariablesMethod.
	self assert: method temporaryVariables first name equals: #method.

	"On the level of CompiledMethod, args include temps. This needs to be fixed"
	method := OrderedCollection >> #do:.
	self assert: method temporaryVariables first name equals: #aBlock.
	self assert: (method hasTemporaryVariableNamed: #aBlock).
	self assert: (method lookupVar: #aBlock) class equals: ArgumentVariable
]

{ #category : 'tests' }
TemporaryVariableTest >> testWriteTemporaryVariablesMethod [
	| tempVar |
	tempVar := thisContext lookupVar: #tempVar.

	tempVar write: 5 inContext: thisContext.
	self assert: tempVar equals: 5
]
