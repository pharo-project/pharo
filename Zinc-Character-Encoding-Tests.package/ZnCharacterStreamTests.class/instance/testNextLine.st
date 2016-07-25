testing
testNextLine
	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStream.
	self assert: stream nextLine equals: 'abc'.
	self assert: stream nextLine  equals: nil.
	stream := ZnCharacterReadStream on: '' asByteArray readStream.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. 
		$b. Character lf. 
		$c } collect: #charCode) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. Character lf.  
		$b. Character cr. Character lf. 
		$c. Character cr. Character lf } collect: #charCode) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. Character lf.  
		Character cr. Character lf. 
		$c. Character cr. Character lf } collect: #charCode) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: ''.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	