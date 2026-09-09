Extension { #name : 'SUnitExtensionsTest' }

{ #category : '*Debugging-Utils-Tests' }
SUnitExtensionsTest >> differentExceptionInShouldRaiseWithExceptionDoTest [

	[ self
		should: [ Error signal ]
		raise: Halt
		withExceptionDo: [ :anException | 
									self 
										assert: false 
										description: 'should:raise:withExceptionDo: handled an exception that should not handle'] ]
	on: Error
	do: [ :anException | anException return: nil ]
]

{ #category : '*Debugging-Utils-Tests' }
SUnitExtensionsTest >> testDifferentExceptionInShouldRaiseWithExceptionDo [
	| testCase testResult |
	testCase := self class selector: #differentExceptionInShouldRaiseWithExceptionDoTest.
	testResult := testCase run.

	self assert: (testResult passed includes: testCase).
	self assertEmpty: testResult errors.
	self assertEmpty: testResult failures.
	self assert: testResult passed size equals: 1
]
