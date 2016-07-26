displaying
label
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'Sent in ';
			nextPutAll: self content methodClass asString;
			nextPutAll: '>>';
			nextPutAll: self content selector ]