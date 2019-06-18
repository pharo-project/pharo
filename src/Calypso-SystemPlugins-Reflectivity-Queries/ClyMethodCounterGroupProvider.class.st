"
I provide ""counters"" method group which put together all methods with installed execution counters
"
Class {
	#name : #ClyMethodCounterGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-ExecutionCounters'
}

{ #category : #'building groups' }
ClyMethodCounterGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'counters' priority: 8.4 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyMethodCounterGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyMethodCallCountersQuery from: aClassScope
]
