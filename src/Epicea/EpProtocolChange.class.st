"
I represent a change in the protocols of a class.
"
Class {
	#name : #EpProtocolChange,
	#superclass : #EpBehaviorChange,
	#instVars : [
		'behavior',
		'protocol'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpProtocolChange class >> behavior: aBehavior protocol: aProtocol [ 
	^ self basicNew 
		initializeWithBehavior: aBehavior
		protocol: aProtocol;
		yourself 
]

{ #category : #accessing }
EpProtocolChange >> behaviorAffected [
	^ behavior
]

{ #category : #accessing }
EpProtocolChange >> behaviorAffectedName [

	^ behavior name
]

{ #category : #testing }
EpProtocolChange >> doesOverride: aCodeChange [ 

	^ aCodeChange isEpProtocolChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName and: [ 
		aCodeChange protocol = self protocol ] ]
]

{ #category : #initialization-release }
EpProtocolChange >> initializeWithBehavior: aBehavior protocol: aProtocol [
	self initialize.
	behavior := aBehavior asEpiceaRingDefinition.
	protocol := aProtocol asSymbol.
]

{ #category : #testing }
EpProtocolChange >> isEpProtocolChange [

	^ true
]

{ #category : #accessing }
EpProtocolChange >> protocol [
	^ protocol
]
