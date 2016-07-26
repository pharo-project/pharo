testing
testBase64
	| short long encoded legalCharactes |
	legalCharactes := (String withAll: ($A to: $Z), ($a to: $z), ($0 to: $9)), '='.
	short := String withAll: ((1 to: 16) collect: [ :each | 'abc' atRandom ]).
	encoded := ZnUtils encodeBase64: short.
	self assert: (ZnUtils decodeBase64: encoded) = short.
	self assert: (encoded allSatisfy: [ :each | legalCharactes includes: each ]).
	"We don't want line breaks!"
	long := String withAll: ((1 to: 64) collect: [ :each | 'abc' atRandom ]).
	encoded := ZnUtils encodeBase64: long.
	self assert: (ZnUtils decodeBase64: encoded) = long.
	self assert: (encoded allSatisfy: [ :each | legalCharactes includes: each ])
