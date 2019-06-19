"
I return all methods which has installed execution counters
"
Class {
	#name : #ClyMethodCallCountersQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-ExecutionCounters'
}

{ #category : #printing }
ClyMethodCallCountersQuery >> description [
	^'Method call counters'
]

{ #category : #testing }
ClyMethodCallCountersQuery >> selectsMethod: aMethod [
	| plugin |
	plugin := self environment getPlugin: ClyReflectiveEnvironmentPlugin.
	^plugin doesMethodHasExecutionCounter: aMethod
]
