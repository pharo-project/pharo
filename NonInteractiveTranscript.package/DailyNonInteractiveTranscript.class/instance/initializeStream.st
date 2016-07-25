private
initializeStream
	"Open the file stream that I write to or connect to #stdout.
	I use the proper line end convention.
	I will append to regular files.
	Overwrtitten to use #fileNameWithDate"
	
	stream := self isStdout 
		ifTrue: [ FileStream stdout ]
		ifFalse: [ FileStream fileNamed: self fileNameWithDate ].
	stream wantsLineEndConversion: true.
	self isStdout
		ifTrue: [ stream converter "Force lazy initialization of converter" ] 
		ifFalse: [ stream setToEnd ].
	^ stream