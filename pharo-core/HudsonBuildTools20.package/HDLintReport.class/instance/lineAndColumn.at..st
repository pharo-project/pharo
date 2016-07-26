private
lineAndColumn: aString at: anInteger
	| line last stream |
	line := 1.
	last := 0.
	stream := aString readStream.
	[ (stream nextLine isNil or: [ anInteger <= stream position ])
		ifTrue: [ ^ line @ (anInteger - last) ].
	last := stream position.
	line := line + 1 ]
		repeat