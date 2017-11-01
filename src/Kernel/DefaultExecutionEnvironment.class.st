"
I am default execution environment which used when nothing special is installed on process.

I am singleton:
	DefaultExecutionEnvironment instance
	
Tools which define specific environments could add polymorphic behaviour to me
"
Class {
	#name : #DefaultExecutionEnvironment,
	#superclass : #ExecutionEnvironment,
	#classInstVars : [
		'instance'
	],
	#category : 'Kernel-Processes'
}

{ #category : #controlling }
DefaultExecutionEnvironment class >> beActive [

	CurrentExecutionEnvironment restoreDefault 
]

{ #category : #controlling }
DefaultExecutionEnvironment class >> beActiveDuring: aBlock [

	^self instance beActiveDuring: aBlock
]

{ #category : #accessing }
DefaultExecutionEnvironment class >> instance [
	^instance ifNil: [ instance := self new ]
]

{ #category : #controlling }
DefaultExecutionEnvironment >> prepareForNewProcess: aProcess [
]
