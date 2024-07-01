"
I am the once behavior that can be added to debug points. 
When I am executed I will disable the debug point to which I am attached.
"
Class {
	#name : 'OnceBehavior',
	#superclass : 'DebugPointMetaBehavior',
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'testing' }
OnceBehavior class >> isAbstract [

	^ false
]

{ #category : 'execution' }
OnceBehavior >> execute [
	self debugPoint enabled: false.
	^true.
]
