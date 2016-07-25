displaying
label
	^ String streamContents: [ :stream |
		stream
			nextPutAll: self content methodClass asString;
			nextPutAll: '>>';
			nextPutAll: self content selector;
			nextPutAll: ' sends #'.
		sentMessages do: [ :message |
			stream nextPutAll: message ] ]