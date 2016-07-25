accessing
next
	"Primitive. Return the next object in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 65>
	"treat me as a FIFO"
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position := position + 1)]