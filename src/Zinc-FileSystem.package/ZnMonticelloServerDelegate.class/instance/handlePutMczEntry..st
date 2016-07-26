private
handlePutMczEntry: request
	| name file |
	name := request uri firstPathSegment.
	file := self directory / name.
	^ ((self isValidMczName: name) and: [ file exists not ]) 
		ifTrue: [
			file writeStreamDo: [ :stream |
				stream binary.
				request entity writeOn: stream ].
			ZnResponse created: request uri ]
		ifFalse: [ ZnResponse badRequest: request ]