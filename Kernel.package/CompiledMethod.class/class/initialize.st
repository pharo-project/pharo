class initialization
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass := OpalEncoderForV3PlusClosures.
	SecondaryBytecodeSetEncoderClass := OpalEncoderForSistaV1