"
I am default execution environment which used when nothing special is installed on process.

I am singleton:
	DefaultExecutionEnvironment instance
	
Tools which define specific environments could add polymorphic behaviour to me
"
Class {
	#name : 'DefaultExecutionEnvironment',
	#superclass : 'ExecutionEnvironment',
	#classInstVars : [
		'instance'
	],
	#category : 'SUnit-Core-Execution-Environments',
	#package : 'SUnit-Core',
	#tag : 'Execution-Environments'
}

{ #category : 'controlling' }
DefaultExecutionEnvironment class >> beActive [

	CurrentExecutionEnvironment restoreDefault
]

{ #category : 'controlling' }
DefaultExecutionEnvironment class >> beActiveDuring: aBlock [

	^self instance beActiveDuring: aBlock
]

{ #category : 'accessing' }
DefaultExecutionEnvironment class >> instance [
	^instance ifNil: [ instance := self new ]
]

{ #category : 'accessing' }
DefaultExecutionEnvironment >> maxTimeForTest: aDuration [

	"just for compatibility with TestExecutionEnvironment, do nothing"
]

{ #category : 'controlling' }
DefaultExecutionEnvironment >> prepareForNewProcess: aProcess [
]

{ #category : 'accessing' }
DefaultExecutionEnvironment >> runTestCase: aTestCase [

	| testEnv |
	testEnv := TestExecutionEnvironment new.

	testEnv beActiveDuring: [
		testEnv runTestCase: aTestCase]
]

{ #category : 'accessing' }
DefaultExecutionEnvironment >> runTestsBy: aBlock [

	TestExecutionEnvironment new beActiveDuring: aBlock
]
