"
A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested.

A Mutex is also more robust to nested critical calls.
For example 

mutex critical: [ mutex critical: [ Trasncript show: 'nested passes through' ] ]

will not deadlock, while a semaphore will. This is why a mutex is also called a recursionLock.

Instance variables:
	semaphore	<Semaphore>		The (primitive) semaphore used for synchronization.
	owner		<Process>		The process owning the mutex.
"
Class {
	#name : #Mutex,
	#superclass : #Object,
	#instVars : [
		'semaphore',
		'owner'
	],
	#category : #'Kernel-Processes'
}

{ #category : #'mutual exclusion' }
Mutex >> critical: aBlock [
	"Execute aBlock only if the receiver is not already used by another process. 
	If it is, wait until the resource is available. 
	If the mutex is already used by the current process, execution proceeds. 
	Answer the block's result."
	
	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == owner ifTrue:[ ^aBlock value ].
	^ semaphore critical: [
			owner := activeProcess.
			aBlock ensure: [ owner := nil ] ].
]

{ #category : #initialization }
Mutex >> initialize [
	super initialize.
	semaphore := Semaphore forMutualExclusion.
]
