convenience
on: readStream do: block
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)