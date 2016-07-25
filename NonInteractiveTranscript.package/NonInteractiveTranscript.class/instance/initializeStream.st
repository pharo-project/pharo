private
initializeStream
	"Open the file stream that I write to or connect to #stdout.
	I use the proper line end convention.
	I will append to regular files."
	
	stream := self isStdout 
		ifTrue: [ FileStream stdout ]
		ifFalse: [ 
			self isStderr
				ifTrue: [ FileStream stderr ]
				ifFalse: [ FileStream fileNamed: self fileName ]].
	stream wantsLineEndConversion: true.
	(self isStdout or: [ self isStderr ])
		ifTrue: [ stream converter "Force lazy initialization of converter" ] 
		ifFalse: [ stream setToEnd ].
	^ stream
	