private
initializeStream
	"Open the file stream that I write to or connect to #stdout.
	I use the proper line end convention.
	I will append to regular files.
	Overwrtitten to use #fileNameWithDate"
	
	stream := self isStdout 
		ifTrue: [ Stdio stdout ]
		ifFalse: [ File named: self fileNameWithDate ].
	self isStdout
		ifFalse: [ stream setToEnd ].
	^ stream