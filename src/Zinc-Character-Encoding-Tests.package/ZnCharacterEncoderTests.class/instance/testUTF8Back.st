testing
testUTF8Back
	| encoder stream |
	encoder := ZnUTF8Encoder new.
	stream := (encoder encodeString: 'Les élèves Françaises') readStream.
	self should: [ encoder backOnStream: stream ] raise: Error.
	4 timesRepeat: [ encoder nextFromStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $é.
	encoder backOnStream: stream.
	self assert: (encoder nextFromStream: stream) equals: $é.
	10 timesRepeat: [ encoder nextFromStream: stream ].
	13 timesRepeat: [ encoder backOnStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $s