private
newResultFor: anExample
	^ GTExampleResult new 
		example: anExample;
		context: self context;
		yourself.