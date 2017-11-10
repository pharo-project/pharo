Extension { #name : #Collection }

{ #category : #'*GT-SUnitDebugger' }
Collection >> gtDebuggerSUnitPrint [

	^ String streamContents: [:s | self asStringOn: s delimiter: Character cr asString]
]
