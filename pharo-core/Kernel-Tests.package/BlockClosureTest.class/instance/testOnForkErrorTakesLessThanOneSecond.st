tests - on-fork
testOnForkErrorTakesLessThanOneSecond
	"Test that if code runs with error, there is fork"
	
	| sema timeout |

	self flag: 'This test is too brittle, failing often on Windows CI'; skip.
		
	self flag: 'The following line makes the test pass under headless linux. Everywhere else this test works'.
	Smalltalk os isUnix
		ifTrue: [ 1 milliSecond wait ].
	
	sema := Semaphore new.
	[ 1/0 ] on: Exception fork: [ sema signal ].
		
	timeout := (sema waitTimeoutSeconds: 1).	
	self assert: timeout == false