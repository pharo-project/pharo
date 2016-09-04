private
basicNext
	"Answer the next byte from this file, or nil if at the end of the file."
	
	| count |
	<primitive: 65>
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count := self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	count = 1
		ifTrue: [ ^buffer1 at: 1 ]
		ifFalse: [ ^nil ]