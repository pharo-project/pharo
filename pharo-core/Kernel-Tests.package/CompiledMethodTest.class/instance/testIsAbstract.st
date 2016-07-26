tests - abstract
testIsAbstract

	self assert: (self class >> #abstractMethod) isAbstract..
	self deny: (self class >> #nonAbstractMethod) isAbstract..
	self deny: (self class >> #shouldNotImplementMethod) isAbstract..