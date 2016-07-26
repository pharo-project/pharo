serialize/materialize
materializeInstanceWith: aDecoder

	| compiledMethod header bytecodesPlusTrailerSize |

	header := aDecoder nextEncodedInt32.
	bytecodesPlusTrailerSize := aDecoder nextEncodedUint16.

	compiledMethod := CompiledMethod newMethod: bytecodesPlusTrailerSize header: header.

	compiledMethod initialPC to: compiledMethod size
		do: [ :index | compiledMethod at: index put: aDecoder nextEncodedByte ].
		
	^ compiledMethod