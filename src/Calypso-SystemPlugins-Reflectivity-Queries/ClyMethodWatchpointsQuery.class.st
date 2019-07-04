"
I return all methods which has installed watchpoints
"
Class {
	#name : #ClyMethodWatchpointsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Watchpoints'
}

{ #category : #printing }
ClyMethodWatchpointsQuery >> description [
	^'Method watchpoints'
]

{ #category : #testing }
ClyMethodWatchpointsQuery >> selectsMethod: aMethod [
	| plugin |
	plugin := self environment getPlugin: ClyReflectiveEnvironmentPlugin.
	^plugin doesMethodHasWatchpoint: aMethod
]
