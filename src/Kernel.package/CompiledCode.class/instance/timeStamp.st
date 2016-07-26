printing
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."
	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	^ SourceFiles timeStampAt: self sourcePointer.