"
I am a root of hierarchy of errors which represent the dirty state of system produced by test.
For example the test can left the running background process or background process can raise an error.

TestExecutionEnvironment checks the system state at the end of test and concrete error is raised if dirty state is detected. 
Those errors are signalled even if test was failed due to domain error. In that case when domain error is handled by SUnit (marking the test result as failed) the another error is signaled to notify about bad system state
"
Class {
	#name : #DirtyTestError,
	#superclass : #Error,
	#category : #'SUnit-Core-Kernel'
}
