tests
testOldBack
	"Test the old behavior of the method back. The method #oldBack is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. The method #oldBack considers that the pointer is *on* an element. (Damien Cassou - 1 August 2007)"
	|stream|
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream oldBack = $b.
