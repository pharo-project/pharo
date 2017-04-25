initialization
useUnixRandomGeneratorSeed
	"Try to seed the receiver using random bytes from a Unix OS' /dev/random.
	Return true if we succeeded, false otherwise.
	Note that this might block until the OS thinks it has enough entropy."

	^ [ '/dev/random' asFileReference binaryReadStreamDo: [ :in | 
			[ seed := (in next: 4) asInteger.
			seed isZero ] whileTrue. "Try again if we ever get a zero value"
			true ] ]
		on: Error
		do: [ false ]