tests
testReadingFromStdoutAfterCommandFinishesDoesNotBlocksVM
	
	"testReadingFromStdoutAfterCommandFinishesDoesNotBlocksVM fails on Travis on OSX. It looks like if another test would have run while this one was sleeping and hence in #assertStreamsInfoWithPrevious: it fails because there are 2 new not-closed files...which I suspect that those are from another running test..."
	Smalltalk platform isMacOS 
		ifFalse: [ super testReadingFromStdoutAfterCommandFinishesDoesNotBlocksVM ]

