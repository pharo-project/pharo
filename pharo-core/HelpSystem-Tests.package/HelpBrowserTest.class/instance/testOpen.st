testing
testOpen
	|browser|
	browser := self defaultTestClass open.
	World doOneCycleNow. 
	browser close 


		