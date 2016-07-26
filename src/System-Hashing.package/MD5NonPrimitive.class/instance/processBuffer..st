private-buffers
processBuffer: aByteArray 
	"Process a 64-byte buffer"
	| saveState data |
	saveState := state collect: [ :item | item copy ].
	data := Array new: 16.
	1 
		to: 16
		do: 
			[ :index | 
			data 
				at: index
				put: (ThirtyTwoBitRegister new 
						reverseLoadFrom: aByteArray
						at: index * 4 - 3) ].
	self rounds: data.
	1 
		to: 4
		do: [ :index | (state at: index) += (saveState at: index) ]