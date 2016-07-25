private
annonymousClassCreator
	^ String streamContents: [ :stream |
		stream 
			nextPutAll: '(FFITypeArray ofType: #';
			print: self objectClass type class;
			nextPutAll: ' size: ';
			print: self objectClass numberOfElements;
			nextPutAll: ')' ]