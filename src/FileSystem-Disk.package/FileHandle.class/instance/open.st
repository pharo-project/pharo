public
open
	self flag: 'TODO: for now we solely rely on the old FileStreams'
	"id := self basicOpen.
	id ifNil: 	[
		reference exists ifFalse: [FileDoesNotExist signalWith: reference].
		self error: 'Unable to open file ' , reference printString]"