tests
testSizeInMemoryCompactClasses
	self skip.
	"One word for the header + one word per instance variable"
	self assert:  (Association key: 'aKey' value: 23) sizeInMemory = 12.

	"One word for the header + one word per instance variable"
	self assert:  Rectangle new sizeInMemory = 12.

