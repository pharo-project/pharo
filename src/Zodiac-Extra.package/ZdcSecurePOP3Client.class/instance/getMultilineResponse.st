private protocol
getMultilineResponse
	"Get a multiple line response to the last command, filtering out LF characters. 
	A multiple line response ends with a line containing only a single period (.) character."

	| response done chunk |
	response := String new writeStream.
	done := false.
	[ done ] whileFalse: [
		chunk := self nextLineFromStream.
		(chunk beginsWith: '.')
			ifTrue: [ response nextPutAll: (chunk copyFrom: 2 to: chunk size); cr ]
			ifFalse: [ response nextPutAll: chunk; cr ].
		done := (chunk = '.') ].
	^ response contents
