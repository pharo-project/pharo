"
I return all methods which has installed execution counters
"
Class {
	#name : #ClyMethodCallCounters,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-ExecutionCounters'
}

{ #category : #printing }
ClyMethodCallCounters >> description [
	^'Method call counters'
]

{ #category : #testing }
ClyMethodCallCounters >> selectsMethod: aMethod [
	| plugin |
	plugin := self environment getPlugin: ClyReflectiveEnvironmentPlugin.
	^plugin doesMethodHasExecutionCounter: aMethod
]
