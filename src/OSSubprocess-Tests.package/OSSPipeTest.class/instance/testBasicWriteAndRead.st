testing
testBasicWriteAndRead
	| pipe string readString |
	string := 'this is a testing string'. 
	pipe := self blockingPipe.
	pipe writer nextPutAll: string; flush.
	readString := pipe reader next: string size.
	self assert: string equals: readString