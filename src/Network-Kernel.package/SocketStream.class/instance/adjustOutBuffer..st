private
adjustOutBuffer: bytesToWrite
	"Possibly grow outBuffer to accommodate the new data.
	Currently grows through doubling when less
	than 1024 bytes are left. If bytesToWrite is even
	larger we double that instead. Never shrinks."

	(outBuffer size - outNextToWrite - bytesToWrite) < 1024 ifTrue: [
		outBuffer := (self streamBuffer: ((outBuffer size max: bytesToWrite) * 2))
						replaceFrom: 1 to: outBuffer size with: outBuffer startingAt: 1]