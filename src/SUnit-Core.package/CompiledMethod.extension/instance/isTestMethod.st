*SUnit-Core
isTestMethod
	"Is the receiver a TestCase test method?"

	self methodClass isTestCase
		ifFalse: [ ^ false ].
	self numArgs isZero
		ifFalse: [ ^ false ].

	"unary selectors starting with 'should' are supposed to be treated as test methods too"
	^ (self selector beginsWith: 'test') or: [ self selector beginsWith: 'should' ]