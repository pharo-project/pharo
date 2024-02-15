"
I am the once behavior that can be added to debug points. 
When I am executed I will disable the debug point to which I am attached.
"
Class {
	#name : 'OnceBehavior',
	#superclass : 'DebugPointBehavior',
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'execution' }
OnceBehavior >> execute [
	self debugPoint enabled: false.
	^true.
]
