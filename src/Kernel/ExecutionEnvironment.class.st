"
I am  special meta object  represent current execution environment for active process.
I allow to manage new forked child processes.

When I am installed on process I will receive message #prepareForNewProcess: for every forked child process.

By default I am not installed on any process. And  DefaultExecutionEnvironment instance is returned for callers which does nothing. 

Tools could define specific environment to provide specific hooks for code execution in context of them.
For example SUnit installs special TestExecutionEnvironment to manage all forked processes during test
"
Class {
	#name : #ExecutionEnvironment,
	#superclass : #Object,
	#category : #'Kernel-Processes'
}

{ #category : #controlling }
ExecutionEnvironment >> activated [
	"Method is called just after installation as current environment.
	Any preparation logic could be here"
]

{ #category : #controlling }
ExecutionEnvironment >> beActiveDuring: aBlock [

	CurrentExecutionEnvironment activate: self for: aBlock
]

{ #category : #controlling }
ExecutionEnvironment >> deactivated [
	"Method is called just after uninstallation when current environment become different.
	Any cleanup logic could be here"
]

{ #category : #controlling }
ExecutionEnvironment >> prepareForNewProcess: aProcess [
	self subclassResponsibility 
]
