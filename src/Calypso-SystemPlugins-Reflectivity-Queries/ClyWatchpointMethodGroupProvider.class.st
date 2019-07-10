"
I provide a ""watchpoints"" method group which put together all methods with installed watchpoints
"
Class {
	#name : #ClyWatchpointMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Watchpoints'
}

{ #category : #'building groups' }
ClyWatchpointMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'watchpoints' priority: 8.4 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyWatchpointMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyMethodWatchpointsQuery from: aClassScope
]
