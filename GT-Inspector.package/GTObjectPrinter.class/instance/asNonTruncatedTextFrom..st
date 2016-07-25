printing
asNonTruncatedTextFrom: anObject
	^ [ Text fromString: (anObject asString replaceAll: String cr with: String space) ] 
			on: Error 
			do: [ Text string: 'error printing' attribute: TextColor red ]