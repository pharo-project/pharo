testing
testApplicationUrlEncodingAddAll
	| entity string data |
	data := Dictionary new.
	data at: 'foo' put: 1 asString.
	data at: 'bar' put: 2 asString.
	entity := ZnApplicationFormUrlEncodedEntity new.
	"Asking for the content length will force an internal computation of the representation"
	self assert: entity contentLength = 0.
	entity addAll: data.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: entity contentLength > 0.
	entity := ZnEntity 
		readFrom: string readStream 
		usingType: ZnApplicationFormUrlEncodedEntity designatedMimeType 
		andLength: string size.
	self assert: (entity at: 'foo') equals: '1'.
	self assert: (entity at: 'bar') equals: '2'