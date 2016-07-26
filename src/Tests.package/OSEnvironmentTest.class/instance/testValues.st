tests
testValues
	| env values |
	
	env := self instance.
	values := env values.
	
	self deny: values isEmpty.
	self assert: values anyOne isString.
	self assert: (env includes: values anyOne)