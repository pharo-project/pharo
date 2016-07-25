*GT-SUnitDebugger
gtDebuggerSUnitPrint

	^ String streamContents: [:s | self asStringOn: s delimiter: Character cr asString]