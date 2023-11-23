"
Merge work
"
Class {
	#name : 'GoferMerge',
	#superclass : 'GoferUpdate',
	#category : 'Gofer-Deprecated-Operations',
	#package : 'Gofer-Deprecated',
	#tag : 'Operations'
}

{ #category : 'private' }
GoferMerge >> defaultModel [

	^ MCVersionMerger new
]

{ #category : 'running' }
GoferMerge >> execute [

	[ self model merge ]
		on: MCMergeResolutionRequest
		do: [ :request |  request autoMerge ].
	self gofer cleanup
]
