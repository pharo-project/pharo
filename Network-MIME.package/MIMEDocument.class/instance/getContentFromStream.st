private
getContentFromStream
	| streamContents |
	streamContents := self contentStream contents.
	self discardContentStream.
	^streamContents