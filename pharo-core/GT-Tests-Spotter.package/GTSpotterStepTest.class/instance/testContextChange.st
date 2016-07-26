tests
testContextChange
	
	self assertSearch: 'a'.
	self assert: previousContext text = ''.
	self denySearch: 'a'.
	
	self assertSearch: 'ab'.
	self assert: previousContext text = 'a'.
	self denySearch: 'ab'.
	
	self assertSearch: 'abc'.
	self assert: previousContext text = 'ab'.
	self denySearch: 'abc'.
	
	self assertSearch: 'ab'.
	self assert: previousContext text = 'abc'.
	self denySearch: 'ab'.
	
	self assertSearch: 'a'.
	self assert: previousContext text = 'ab'.
	self denySearch: 'a'.
	
	self assertSearch: ''.
	self assert: previousContext text = 'a'.
	
	self flag: 'because the decision for triggering a search is made in multiple places, an empty input will always trigger a search - this should be simplified - one place to make the decision GTSpotter>>#setText:from: <-> #Context>>#matches:'.
	self assertSearch: ''.
	self assertSearch: ''.
	