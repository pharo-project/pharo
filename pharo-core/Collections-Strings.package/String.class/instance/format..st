formatting
format: collection 
	"Format the receiver by interpolating elements from collection, as in the following examples:  
	'Today is {1}.' format: {Date today}.
	'Today is {date}.' format: (Dictionary with: #date->Date today). 
	'In {1} you can escape \{ by prefixing it with \\' format: {'strings'}.  "
	
	^ self class new: self size streamContents: [ :result | | stream |
		stream := self readStream.
		[ stream atEnd ] whileFalse: [ | currentChar | 
			(currentChar := stream next) == ${
				ifTrue: [ | expression index | 
					expression := stream upTo: $}.
					index := Integer readFrom: expression ifFail: [ expression ].
					result nextPutAll: (collection at: index) asString ]
				ifFalse: [
					currentChar == $\
						ifTrue: [ stream atEnd ifFalse: [ result nextPut: stream next ] ]
						ifFalse: [ result nextPut: currentChar ] ] ] ]