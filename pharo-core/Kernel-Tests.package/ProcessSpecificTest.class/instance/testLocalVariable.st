testing
testLocalVariable

	| s1 s2 p1stopped p2stopped |
	s1 := Semaphore new.
	s2 := Semaphore new.
	p1stopped := p2stopped := false.
	[
		self checkLocal: 0.
		TestLocalVariable value: 1.
		self checkLocal: 1.
		(Delay forMilliseconds: 30) wait.
		self checkLocal: 1.
		TestLocalVariable value: 2.
		self checkLocal: 2.
		p1stopped := true.
		s1 signal.
	] fork.

	[
		(Delay forMilliseconds: 30) wait.
		self checkLocal: 0.
		TestLocalVariable value: 3.
		self checkLocal: 3.
		(Delay forMilliseconds: 30) wait.
		self checkLocal: 3.
		TestLocalVariable value: 4.
		self checkLocal: 4.
		p2stopped := true.
		s2 signal.
	] fork.

	"Set a maximum wait timeout so that the test case will complete 
	 even if the processes fail to signal us."
	s1 waitTimeoutMSecs: 5000.
	s2 waitTimeoutMSecs: 5000.
	self assert: p1stopped.
	self assert: p2stopped.
