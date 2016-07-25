tests
testConditions
	| condition newCondition |
	condition := RBCondition new 
				type: #false
				block: [false]
				errorString: 'false'.
	condition errorMacro: '<1?true:false>'.
	self deny: condition check.
	self assert: condition errorString = 'false'.
	self assert: condition not check.
	self assert: condition printString = 'false'.
	self assert: condition not printString = 'NOT false'.
	self deny: (condition not & condition) check.
	self 
		assert: (condition not & condition) printString = 'NOT false & false'.
	self assert: (condition & condition) not check.
	self assert: (condition | condition not) check.
	self deny: (newCondition := condition | condition) check.
	self assert: newCondition errorString = 'false AND false'.
	self assert: (condition not | condition not) check.
	self deny: (newCondition := condition & condition) check.
	self assert: newCondition errorString = 'false'.
	self assert: (condition not & condition not) check.
	self assert: (condition & condition) errorString = 'false OR false'