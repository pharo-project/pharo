operation
nextLineIfNone: aBlock
	| span |
	
	span := self lineEndSpan.
	span isAtEnd ifTrue: [ ^ aBlock value ].
	^ span next next