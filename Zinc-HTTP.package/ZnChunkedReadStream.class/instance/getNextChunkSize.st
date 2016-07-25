private
getNextChunkSize
	| line |
	(line := (ZnLineReader on: stream) nextLine) isEmpty ifTrue: [ ^ 0 ].
	^ Integer readFrom: (line copyUpTo: $;) base: 16