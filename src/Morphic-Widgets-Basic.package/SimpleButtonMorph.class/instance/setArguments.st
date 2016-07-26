menu
setArguments
	| newArgs newArgsArray |
	newArgs := UIManager default
		request:
			'Please type the arguments to be sent to the target
when this button is pressed separated by periods' translated
		initialAnswer: (String streamContents: [ :str | arguments do: [ :arg | arg printOn: str. str nextPutAll: '. ' ] ]).
	newArgs isEmptyOrNil ifFalse: [ 
			newArgsArray := self class compiler
				source: '{' , newArgs , '}';
				receiver: self;
				evaluate.
			self arguments: newArgsArray ]