serialize/materialize
materializeInstanceWith: aDecoder

	^ theClass newFromFrameSize: aDecoder nextEncodedByte
