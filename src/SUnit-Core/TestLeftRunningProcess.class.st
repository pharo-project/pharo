"
I notify users about dirty system state when test left running background processes. 
In practice it could affect the execution of next tests and generaly it could break the system when test suite is complete

"
Class {
	#name : #TestLeftRunningProcess,
	#superclass : #DirtyTestError,
	#category : #'SUnit-Core-Kernel'
}
