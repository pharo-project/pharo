private
print: aString withAttributes: attributes on: stream

	stream
		withAttributes: attributes 
		do: [ 
			stream nextPutAll: aString.
			(aString notEmpty and: [ 
				aString last = Character cr or: [
					aString endsWith: String crlf ] ]) 
						ifFalse: [ stream cr ] ]