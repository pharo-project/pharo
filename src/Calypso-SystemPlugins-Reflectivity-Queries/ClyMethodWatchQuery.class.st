"
I return all methods which has installed watchpoints
"
Class {
	#name : #ClyMethodWatchQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Watchpoints'
}

{ #category : #printing }
ClyMethodWatchQuery >> description [
	^'Method watchpoints'
]

{ #category : #testing }
ClyMethodWatchQuery >> selectsMethod: aMethod [
	| plugin |
	plugin := self environment getPlugin: ClyReflectiveEnvironmentPlugin.
	^plugin doesMethodHasWatch: aMethod
]
