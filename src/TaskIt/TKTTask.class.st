"
! TKTTask

TaskIT's main abstraction are, as the name indicates it, tasks. A task is a unit of execution. By splitting the execution of a program in several tasks, TaskIT can run those tasks concurrently, synchronize their access to data, or order even help in ordering and synchronizing their execution.

!! First Example

Launching a task is as easy as sending the message schedule to a block closure, as it is used in the following first code example:

[[[language=smalltalk
[ 1 + 1 ] schedule.
]]]

The selector name schedule is chosen in purpose instead of others such as run, launch or execute. TaskIT promises you that a task will be eventually executed, but this is not necessarilly right away. In other words, a task is scheduled to be executed at some point in time in the future.
This first example is however useful to clarify the first two concept but it remains too simple. We are schedulling a task that does nothing useful, and we cannot even observe it's result (yet). Let's explore some other code snippets that may help us understand what's going on.

The following code snippet will schedule a task that prints to the Transcript. Just evaluating the expression below will make evident that the task is actually executed. However, a so simple task runs so fast that it's difficult to tell if it's actually running concurretly to our main process or not.

[[[language=smalltalk
[ 'Happened' logCr ] schedule.
]]]

The real acid test is to schedule a long-running task. The following example schedules a task that waits for a second before writing to the transcript. While normal synchronous code would block the main thread, you'll notice that this one does not.

[[[language=smalltalk
[ 1 second wait.
'Waited' logCr ] schedule.
]]]

! Valuables as Tasks

We have been using so far block closures as tasks. Block closures are a handy way to create a task since they implictly capture the context: they have access to self and other objects in the scope. However, blocks are not always the wisest choice for tasks. Indeed, when a block closure is created, it references the current context with all the objects in it and its sender contexts, being a potential source of memory leaks.

The good news is that TaskIt tasks can be represented by almost any object. A task, in TaskIT's domain are valuable objects i.e., objects that will do some computation when they receive the value message. Actually, the messages schedule and future we just used are a syntax sugar for:

[[[language=smalltalk
(TKTTask valuable: [ 1 logCr ]) schedule.
]]]

We can then create tasks using message sends or weak message sends:

[[[language=smalltalk
TKTTask valuable: (WeakMessageSend receiver: Object new selector: #yourself).
TKTTask valuable: (MessageSend receiver: 1 selector: #+ arguments: { 7 }).
]]]

Or even create our own task object:

[[[language=smalltalk
Object subclass: #MyTask
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyPackage'.

MyTask >> value
    ^ 100 factorial
]]]

and use it as follows:

[[[language=smalltalk
TKTTask valuable: MyTask new.
]]]
"
Class {
	#name : #TKTTask,
	#superclass : #Object,
	#category : #'TaskIt-Kernel'
}

{ #category : #'instance creation' }
TKTTask class >> valuable: aValuable [

	^ TKTGenericTask new
		valuable: aValuable;
		yourself
]

{ #category : #running }
TKTTask >> asTask [
	^ self
]

{ #category : #executing }
TKTTask >> configureTaskExecution: execution with: aRunner [
	execution runner: aRunner.
	execution task: self.
	^ execution
]

{ #category : #running }
TKTTask >> evaluateOn: anExecution [
	self subclassResponsibility
]

{ #category : #running }
TKTTask >> future [
	^ self future: TKTConfiguration runner
]

{ #category : #running }
TKTTask >> future: executor [
	^ executor privateFuture: self
]

{ #category : #executing }
TKTTask >> futureTaskExecutionFor: aRunner [
	^ self configureTaskExecution: TKTFutureExecution new with: aRunner
]

{ #category : #running }
TKTTask >> schedule [
	
	self schedule: TKTConfiguration runner
]

{ #category : #running }
TKTTask >> schedule: executor [
	executor privateSchedule: self 
]

{ #category : #executing }
TKTTask >> scheduleTaskExecutionFor: aRunner [
	^ self configureTaskExecution: TKTTaskExecution new with: aRunner
]
