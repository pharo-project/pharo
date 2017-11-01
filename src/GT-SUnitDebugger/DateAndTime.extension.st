Extension { #name : #DateAndTime }

{ #category : #'*GT-SUnitDebugger' }
DateAndTime >> gtDebuggerSUnitPrint [

	^ String streamContents: [:s | self printSeparateDateAndTimeOn: s]
]
