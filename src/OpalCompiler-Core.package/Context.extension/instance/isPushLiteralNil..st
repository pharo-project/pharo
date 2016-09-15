*OpalCompiler-Core
isPushLiteralNil: aPC
	^ (self method at: aPC) = method encoderClass pushNilByte