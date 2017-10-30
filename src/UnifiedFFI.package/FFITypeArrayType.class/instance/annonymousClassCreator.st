private
annonymousClassCreator
	^ String streamContents: [ :stream | 
		stream
			nextPutAll: '(FFITypeArray ofType: ';
			print: (self objectClass type isPointer
				ifTrue: [ self externalTypeWithArity printString ]
				ifFalse: [ '#' , self objectClass type class ]);
			nextPutAll: ' size: ';
			print: self objectClass numberOfElements;
			nextPutAll: ')' ]