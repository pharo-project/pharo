construction
add: aString target: anObject selector: aSelector argumentList: anArray
	^ self add: aString translated action: (MessageSend receiver: anObject selector: aSelector arguments: anArray)