testing
uses: aNode 
	aNode = body ifFalse: [^false].
	^parent isMessage 
		ifTrue: 
			[(#(#ifTrue:ifFalse: #ifTrue: #ifFalse: #ifFalse:ifTrue:) 
				includes: parent selector) not 
				or: [parent isUsed]]
		ifFalse: [self isUsed]