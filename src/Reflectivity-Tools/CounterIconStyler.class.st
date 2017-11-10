"
I am rendering ExecutionCounters in the editor.
"
Class {
	#name : #CounterIconStyler,
	#superclass : #IconStyler,
	#category : #Reflectivity-Tools-Watchpoints
}

{ #category : #defaults }
CounterIconStyler >> iconBlock: aNode [
	^[ :seg | ExecutionCounter removeFrom: aNode. seg delete]
]

{ #category : #defaults }
CounterIconStyler >> iconFor: aNode [
	^ self iconNamed: #classIcon 
]

{ #category : #defaults }
CounterIconStyler >> iconLabelBlock: aNode [

	^ [aNode counter count asString]
]

{ #category : #testing }
CounterIconStyler >> shouldStyleNode: aNode [
	^aNode hasExecutionCounter
]
