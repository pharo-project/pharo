private building
buildMethodTopicsOn: topic for: aClass
	|stream methodComment|
	stream := String new writeStream.
	aClass selectors asSortedCollection do: [:selector | 
		methodComment := (aClass>>selector) comment.
		addMethodsWithoutComment | methodComment notNil ifTrue: [ 
			stream 
				nextPutAll: aClass name;
				nextPutAll: '>>';
				nextPutAll: selector asString;
				cr.		

		methodComment ifNil: [ methodComment := 'Method has no comment.' ].
		stream nextPutAll: methodComment;cr;cr ] ].
	
	topic contents: stream contents.
	
	