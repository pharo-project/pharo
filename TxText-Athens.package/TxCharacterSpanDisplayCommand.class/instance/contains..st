testing
contains: aTextPosition

	"this one is tricky and should be covered by tests, because
	start and stop here is start charater index and stop character index, but not positions
	 "
	
	self flag: #coverByTest.
	
	^ aTextPosition span == span and: [ | spanPos |  
			spanPos := aTextPosition spanPosition.
			spanPos between: start-1 and: stop ]