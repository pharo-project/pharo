construction
add: aString selector: aSelector argumentList: anArray
	^ self add: aString action: (MessageSend receiver: model selector: aSelector arguments: anArray)