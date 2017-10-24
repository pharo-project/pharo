private
initializeStream
	"Open the file stream that I write to or connect to #stdout.
	I use the proper line end convention.
	I will append to regular files."
	
	stream := self isStdout 
		ifTrue: [ Stdio stdout ]
		ifFalse: [ 
			self isStderr
				ifTrue: [ Stdio stderr ]
				ifFalse: [ File named: self fileName ]].
	(self isStdout or: [ self isStderr ])
		ifFalse: [ stream setToEnd ].
	^ stream