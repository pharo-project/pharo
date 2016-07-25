read, write, position
upToEndOfFile
	"Answer the remaining elements in the pipe. Use #atEndOfFile to
	determine end of file status with feof(), required for reliable end of
	file test on OS pipes. Compare #upToEnd, which uses the generic end
	of file test in FilePlugin."

	| stream delay string |
	stream := WriteStream on: String new.
	delay := Delay forMilliseconds: 200.
	[(string := self next: 2000) isEmpty
		ifTrue: [self atEndOfFile
						ifTrue: [^ stream contents]
						ifFalse: [delay wait]]
		ifFalse: [stream nextPutAll: string]] repeat
