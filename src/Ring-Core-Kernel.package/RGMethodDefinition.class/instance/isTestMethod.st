SUnit-support
isTestMethod
	"Is the receiver a TestCase test method?"

	| selectorString |
	self methodClass isTestCase
		ifFalse: [ ^ false ].
	self numArgs isZero
		ifFalse: [ ^ false ].
	selectorString := self selector asString.

	"for some reason unary selectors starting with 'should' are supposed
	to be treated as test methods"
	^ (selectorString beginsWith: 'test') or: [ selectorString beginsWith: 'should' ]