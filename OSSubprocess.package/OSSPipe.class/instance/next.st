read, write, position
next
	"Answer the next object accessible by the receiver."

	| character |
	nextChar isNil
		ifTrue:
			[^ [reader next]
				on: Error
				do: [nil]]
		ifFalse:
			[character := nextChar.
			nextChar := nil.
			^ character]
