accessing
warningMessage

	^ String streamContents: [ :stream |
		stream	 
			nextPutAll: self methodClass name;
			nextPutAll: '>>';
			nextPutAll: self node methodNode selector;
			nextPutAll: '(';
			nextPutAll: self node name;
			nextPutAll: ' is shadowed)'	
	]