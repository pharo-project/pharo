testing
testNextPutAllStartingAt
	"Test that non-ascii ByteString characters are converted correctly when using next:putAll:startingAt:"
	|converter stream source|
	converter := UTF8TextConverter new.
	stream := (String new: 10) writeStream.
	source := 'abcdefågh'. "Notice å at 7, which is a non-ascii character in latin1-range, hence ByteString encoded"
converter next: 5 putAll: source startingAt: 5 toStream: stream.
"C3A5 is twobyte utf8-encoding of å"
self assert: stream contents asByteArray = #[16r65 16r66 16rC3 16rA5 16r67 16r68] 