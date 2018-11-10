"
I return all methods which has installed watchpoints
"
Class {
	#name : #ClyMethodWatchpoints,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Watchpoints'
}

{ #category : #printing }
ClyMethodWatchpoints >> description [
	^'Method watchpoints'
]

{ #category : #testing }
ClyMethodWatchpoints >> selectsMethod: aMethod [
	| plugin |
	plugin := self environment getPlugin: ClyReflectiveEnvironmentPlugin.
	^plugin doesMethodHasWatchpoint: aMethod
]
