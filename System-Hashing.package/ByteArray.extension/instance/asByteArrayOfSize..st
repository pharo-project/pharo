*System-Hashing-Core
asByteArrayOfSize: size
	"
		'34523' asByteArray asByteArrayOfSize: 100.

	(((
		| repeats bytes | 
		repeats := 1000000.
		bytes := '123456789123456789123456789123456789123456789123456789' asByteArray.
		 [repeats timesRepeat: (bytes asByteArrayOfSize: 1024) ] timeToRun.
	)))"

	| bytes |
	size < self size
		ifTrue: [^ self error: 'bytearray bigger than ', size asString].
	bytes := self asByteArray.
	^ (ByteArray new: (size - bytes size)), bytes
