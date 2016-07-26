private
hasMagicNumber: aByteArray 
	| position |
	position := stream position.
	(stream size - position >= aByteArray size and: [ (stream next: aByteArray size) = aByteArray ]) ifTrue: [ ^ true ].
	stream position: position.
	^ false