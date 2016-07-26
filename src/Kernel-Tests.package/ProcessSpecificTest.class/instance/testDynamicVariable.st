testing
testDynamicVariable

	| s1 s2 p1stopped p2stopped |
	s1 := Semaphore new.
	s2 := Semaphore new.
	p1stopped := p2stopped := false.
	[
		TestDynamicVariable value: 1 during:[
			self checkDynamic: 1.
			(Delay forMilliseconds: 30) wait.
			self checkDynamic: 1.
			TestDynamicVariable value: 3 during:[
				(Delay forMilliseconds: 30) wait.
				self checkDynamic: 3
			].
			self checkDynamic: 1.
		].
		self checkDynamic: nil.
		p1stopped := true.
		s1 signal.
	] fork.

	[
		TestDynamicVariable value: 2 during:[
			self checkDynamic: 2.
			(Delay forMilliseconds: 30) wait.
			self checkDynamic: 2.
		].
		self checkDynamic: nil.
		p2stopped := true.
		s2 signal.
	] fork.

	"Set a maximum wait timeout so that the test case will complete 
	 even if the processes fail to signal us."
	s1 waitTimeoutSeconds: 2.
	s2 waitTimeoutSeconds: 2.
	self assert: p1stopped.
	self assert: p2stopped.