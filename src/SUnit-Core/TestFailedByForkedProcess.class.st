"
I notify users about failed processes which were forked during the test.
I prevent the test to complete successfully when there was no errors from the main test process but some of background activities fail.

In practice I ensure no green tests which ""spawn background debuggers""
"
Class {
	#name : #TestFailedByForkedProcess,
	#superclass : #DirtyTestError,
	#category : #'SUnit-Core-Kernel'
}
