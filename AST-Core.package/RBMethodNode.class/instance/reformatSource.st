private
reformatSource
	| stream newSource newTree |
	stream := (String new: source size + 100) writeStream.
	stream
		nextPutAll: (source
			copyFrom: (replacements
				inject: 1
				into: [ :sum :each | 
					stream
						nextPutAll: (source copyFrom: sum to: each startPosition - 1);
						nextPutAll: (each string).
					each stopPosition + 1 ])
			to: source size).
	newSource := stream contents.
	newTree := RBParser 
		parseMethod: newSource 
		onError: [ :msg :pos | ^ self formattedCode ].
	self = newTree
		ifFalse: [ ^ self formattedCode ].
	^ newSource