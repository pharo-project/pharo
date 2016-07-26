printing
configMethodOn: aStream for: spec selector: selector last: last indent: indent
	spec == nil
		ifTrue: [ ^ self ].
	aStream
		tab: indent;
		nextPutAll: 'spec ' , selector , ' [';
		cr.
	spec configMethodOn: aStream indent: indent + 1.
	aStream nextPutAll: ' ].'.
	last
		ifFalse: [ aStream cr ]