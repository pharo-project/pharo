printing
printConfigurations: configurations

	self inform: (String streamContents:  [ :s|
		s
			nextPutAll: 'Found ';
			print: configurations size;
			nextPutAll: ' Configuration';
			nextPutAll: (configurations size = 1 ifTrue: [':'] ifFalse: ['s:']) ]).
	
	configurations do: [ :name | 
			self << name. 
			self stdout lf ].
	