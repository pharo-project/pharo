splitjoin
split: aString indicesDo: aBlock
	| lastPosition |

	stream := aString readStream.
	lastPosition := stream position.
	
	[ self searchStream: stream ] whileTrue: [ 
		aBlock value: lastPosition+1 value: (self subBeginning: 1) first.
		[ lastPosition < stream position ] assertWithDescription: 'Regex cannot match null string'.
		lastPosition := stream position ].
	
	aBlock value: lastPosition + 1 value: aString size.