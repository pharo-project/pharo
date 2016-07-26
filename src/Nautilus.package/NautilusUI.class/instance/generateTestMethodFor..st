widget method
generateTestMethodFor: aMethod

	"generate a test method for aMethod and return the generated method"
	| class testMethodName|
	testMethodName := aMethod methodClass name, 'Test'.
	class := self model classes detect: [:e | e name = testMethodName] ifNone: [ self createTestForClass: aMethod methodClass ].
	class
		ifNotNil:[ 
			class compile: (self generateSourceCodeForTestFrom: aMethod) classified: 'tests-',aMethod category.
			^ class >> (self buildTestSelectorFor: aMethod) asSymbol ].
	^ nil