private
readFrom: readStream usingType: contentType andLength: length
	| entityClass selector |
	entityClass := self isStreaming 
		ifTrue: [ ZnStreamingEntity ]
		ifFalse: [ ZnEntity ].
	selector := self isBinary 
		ifTrue: [ #readBinaryFrom:usingType:andLength: ]
		ifFalse: [ #readFrom:usingType:andLength: ].
	^ entityClass perform: selector 
		with: readStream
		with: contentType 
		with: length