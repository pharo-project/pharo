testing
testLineEndingsWith: anEncodingName
	 | byteMap |
	"Make a map of different endings, and the expected byte pattern when three cr's are added using it"
	byteMap := Dictionary new.
	byteMap at: #cr put: #[13 13 13];
			at: #lf put: #[10 10 10];
			at: #crlf put: #[13 10 13 10 13 10].
	byteMap keysDo: [:lineEnding  |
		FileStream forceNewFileNamed: self lineEndTestFile do: [:stream | 
			stream 
				lineEndConvention: lineEnding;
				converter: (TextConverter newForEncoding: anEncodingName).
			"Test 3 cases: Use of nextPut, nextPutAll:, and nextPutAll with WideStrings"
			stream nextPut: Character cr;
					nextPutAll: Character cr asString;
					nextPutAll: Character cr asString asWideString.].
		FileStream oldFileNamed: self lineEndTestFile do: [:stream | | bytes expected| 
			bytes := stream binary; contents.
			self assert: bytes equals: (byteMap at: lineEnding)]]