accessing
compiledMethodTrailer
	^ compiledMethodTrailer ifNil: [ compiledMethodTrailer := CompiledMethodTrailer empty ]