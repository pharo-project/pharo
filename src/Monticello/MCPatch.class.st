"
I represent a set of patch operations which can be applied by sending message applyTo: to my instances. 
"
Class {
	#name : 'MCPatch',
	#superclass : 'Object',
	#instVars : [
		'operations'
	],
	#category : 'Monticello-Patching',
	#package : 'Monticello',
	#tag : 'Patching'
}

{ #category : 'instance creation' }
MCPatch class >> fromBase: baseSnapshot target: targetSnapshot [
	^ self new initializeWithBase: baseSnapshot target: targetSnapshot
]

{ #category : 'applying' }
MCPatch >> applyTo: anObject [
	operations do: [:ea | ea applyTo: anObject].

]

{ #category : 'intializing' }
MCPatch >> initializeWithBase: baseSnapshot target: targetSnapshot [
	| base target |	
	operations := OrderedCollection new.
	base := MCDefinitionIndex definitions: baseSnapshot definitions.
	target := MCDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions 
		do: [ :t |
			base
				definitionLike: t
				ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (MCModification of: b to: t)]]
				ifAbsent:  [operations add: (MCAddition of: t)] ]
		displayingProgress: 'Diffing...'.
		
	base definitions do: [:b |
		target
			definitionLike: b
			ifPresent: [:t |]
			ifAbsent: [operations add: (MCRemoval of: b)]]		
]

{ #category : 'querying' }
MCPatch >> isEmpty [
	^ operations isEmpty
]

{ #category : 'accessing' }
MCPatch >> operations [
	^ operations
]
