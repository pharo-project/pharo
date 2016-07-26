private
formatMessageNodeInAMessageStreamFor: aMessageNode
	| messageStream |
	messageStream := WriteStream on: (String new: 100).
	self
		with: aMessageNode selectorParts
		and: aMessageNode arguments
		do: [ :selector :arg | 
			messageStream
				nextPutAll: selector;
				space;
				nextPutAll: (self formattedSourceFor: arg) ]
		separatedBy: [ messageStream space ].
		^ messageStream 