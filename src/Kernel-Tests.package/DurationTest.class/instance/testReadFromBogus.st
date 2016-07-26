tests
testReadFromBogus

	self should: [Duration readFrom: '+0:01:02' readStream] raise: Error.
	"Seconds should be supplied as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"