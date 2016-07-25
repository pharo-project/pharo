initialize-release
readFrom: stream
	"We parse from stream, limited to content length if available."
	
	| readStream |
	representation := nil.
	contentLength isNil
		ifTrue: [ readStream := ZnCharacterReadStream on: stream encoding: 'ascii' ]
		ifFalse: [ (readStream := ZnLimitedReadStream on: stream limit: contentLength) ascii ].
	fields := ZnResourceMetaUtils parseQueryFrom: readStream