tests
testCaseSensitiveMap
	| map colon o |
	map := String classPool at: #CaseSensitiveOrder.
	colon := map at: $: asciiValue + 1.
	o := map at: $O asciiValue + 1.
	self assert: colon > o