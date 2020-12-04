"
I provide a ""watchpoints"" method group which put together all methods with installed watchpoints
"
Class {
	#name : #ClyWatchMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Watchpoints'
}

{ #category : #'building groups' }
ClyWatchMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'watchpoints' priority: 8.4 on: aMethodQuery 
]

{ #category : #'building groups' }
ClyWatchMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyMethodWatchQuery from: aClassScope
]
