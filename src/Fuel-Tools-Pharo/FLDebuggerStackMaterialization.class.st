"
I am signalled when a dead stack is materialized from Fuel. My default action is to directly request the opening of a debugger on the post-mortem stack.
"
Class {
	#name : #FLDebuggerStackMaterialization,
	#superclass : #Exception,
	#instVars : [
		'interruptedContext'
	],
	#category : #'Fuel-Tools-Pharo-Debugger'
}

{ #category : #handling }
FLDebuggerStackMaterialization >> defaultAction [ 
	(OupsDebugRequest newForContext: interruptedContext)
			process: Processor activeProcess;
			label: 'External stack';
			submit 
]

{ #category : #accessing }
FLDebuggerStackMaterialization >> interruptedContext: anObject [

	interruptedContext := anObject
]
