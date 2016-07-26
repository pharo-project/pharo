private-scanning
scanNumber
	| start number stop string |
	start := stream position.
	stream position: start - 1.
	number := [NumberParser parse: stream] on: Error do: [:err | self scannerError: err messageText].
	stop := stream position.
	stream position: start - 1.
	string := stream next: stop - start + 1.
	stream position: stop.
	self step.
	^RBNumberLiteralToken 
		value: number
		start: start
		stop: stop
		source: string