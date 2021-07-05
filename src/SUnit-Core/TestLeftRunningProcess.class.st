"
I notify users about dirty system state when test left running background processes. 
The process left running after the test could affect the execution of other tests and generally it could break the system when test suite is complete

"
Class {
	#name : #TestLeftRunningProcess,
	#superclass : #DirtyTestError,
	#category : #'SUnit-Core-Kernel'
}
