"
Merge work
"
Class {
	#name : 'GoferMerge',
	#superclass : 'GoferUpdate',
	#category : 'Gofer-Core-Operations',
	#package : 'Gofer-Core',
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
