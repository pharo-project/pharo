"
I am rendering ExecutionCounters in the editor.
"
Class {
	#name : 'CounterIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Watches',
	#package : 'Reflectivity-Tools',
	#tag : 'Watches'
}

{ #category : 'testing' }
CounterIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
CounterIconStyler >> iconBlock: aNode [
	^[ :seg | ExecutionCounter removeFrom: aNode. seg delete]
]

{ #category : 'defaults' }
CounterIconStyler >> iconFor: aNode [
	^ self iconNamed: #class
]

{ #category : 'defaults' }
CounterIconStyler >> iconLabelBlock: aNode [

	^ [aNode counter count asString]
]

{ #category : 'testing' }
CounterIconStyler >> shouldStyleNode: aNode [
	^aNode hasExecutionCounter
]
