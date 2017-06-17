accessing
message
	^ message ifNil: [ message := 'empty message' ]