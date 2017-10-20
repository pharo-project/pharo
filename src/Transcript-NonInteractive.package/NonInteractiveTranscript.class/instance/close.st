streaming
close
	"Close the receiver, indicating it is not longer needed"
	
	self critical: [ 
		stream ifNotNil: [ 
			(self isStdout or: [ self isStderr ])
				ifTrue: [ stream flush ]
				ifFalse: [ stream close ].
			stream := nil ] ]