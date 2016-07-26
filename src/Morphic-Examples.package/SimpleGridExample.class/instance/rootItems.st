accessing
rootItems
	^ rootItems ifNil: [rootItems := (1 to: 1000)
		collect: [:i | PersonDataExample
				firstName: 'Person' , i asString
				secondName: 'Person' , i asString , ' second name'
				age: i
				married: false] ]