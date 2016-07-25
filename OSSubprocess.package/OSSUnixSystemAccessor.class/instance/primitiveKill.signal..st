signals - primitives
primitiveKill: aPid signal: aSignal

	^ self ffiCall: #( int kill(int aPid, int aSignal) )
	