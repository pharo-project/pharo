tests - printing
testPrintOn

	| string |
	string := String streamContents: [:stream | nil printOn: stream].
	self assert: (string = 'nil').