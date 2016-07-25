accessing response
contents 
	"Answer the contents or HTTP payload of the last response, 
	assuming the current request was executed.
	If I have a contentReader, it will be used to compute the contents.
	Otherwise I delegate to ZnResponse>>#contents, unless I am #streaming,
	in which case I return the stream of the streaming entity."

	^ response ifNotNil: [ 
		self contentReader 
			ifNil: [ 
				self streaming
					ifTrue: [  response entity stream ]
					ifFalse: [ response contents ] ]
			ifNotNil: [ :reader | reader value: response entity ] ]  