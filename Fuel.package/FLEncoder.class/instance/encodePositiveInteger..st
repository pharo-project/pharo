encoding
encodePositiveInteger: anInteger
	"Append to the receiver an Integer as the next 4 bytes."
	1 to: 4 do: [:i | stream nextPut: (anInteger digitAt: 4+1-i)].