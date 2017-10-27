tests - examples
testExample1

	| producer1 producer2  monitor goal work counter goalReached finished |
	goal := (1 to: 1000) asOrderedCollection.
	work := OrderedCollection new.
	counter := 0.
	goalReached := false.
	finished := Semaphore new.
	monitor := Monitor new.

	producer1 := [
       [monitor critical:
             [monitor waitUntil: [counter \\5 = 0].
              goalReached or: [work add: (counter := counter + 1)].
              goalReached := counter >= goal size.
              monitor signal
            ].
           goalReached
          ]
             whileFalse.
         finished signal.
	].

	producer2 := [
         [monitor critical:
                [monitor waitWhile: [counter \\5 = 0].
                 goalReached or: [work add: (counter := counter + 1)].
                 goalReached := counter >= goal size.
                 monitor signal].
         goalReached
       ] whileFalse.
     finished signal
	].

	producer1 forkAt: Processor userBackgroundPriority.
	producer2 forkAt: Processor userBackgroundPriority.

	finished wait; wait.
	self assert: goal = work