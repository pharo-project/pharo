testing
testAllClassesUsingSharedPool
	"self debug: #testAllClassesUsingSharedPool"

	| res |
	res := SystemNavigation new allClassesUsingSharedPool: 'ChronologyConstants'.
	self assert: ({Date. DateAndTime. Duration. Month. Time. TimeZone. Week} difference: res) isEmpty