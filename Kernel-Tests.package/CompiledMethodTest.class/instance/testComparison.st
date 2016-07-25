tests - accessing
testComparison
	| method1 method2 |
	method1 := Float class >> #nan.
	method2 := thisContext method.
	
	self assert: method1 = method1.
	self assert: method2 = method2.
	self deny: method1 = method2.
	self deny: method2 = method1.
	
	Object methods do: [ :each |
		self deny: method1 = each.
		self deny: each = method1.
		self deny: method2 = each.
		self deny: each = method2 ]