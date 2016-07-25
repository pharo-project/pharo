private
print: aString withAttributes: attributes on: stream

	stream
		withAttributes: attributes 
		do: [ stream nextPutAll: aString ]