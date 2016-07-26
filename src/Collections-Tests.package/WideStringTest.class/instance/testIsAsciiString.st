testing
testIsAsciiString
	"Non-regression for https://pharo.fogbugz.com/f/cases/15232 "
	self assert: '' asWideString isAsciiString = true.
	self assert: 'abcdefGHIJKL 98765,./@#%$' asWideString isAsciiString = true.

	self assert: 'éÀ' asWideString isAsciiString = false.