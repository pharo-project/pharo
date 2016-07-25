read, write, position
next: anInteger 
	"Answer the next anInteger elements of my collection."

	| character stream |
	stream := WriteStream on: String new.
	(1 to: anInteger) do:
		[:index |
		character := self next.
		character isNil
			ifTrue: [^ stream contents]	
			ifFalse: [stream nextPut: character. false]].
	^ stream contents
