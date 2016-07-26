convenience
on: writeStream do: block
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result