Extension { #name : 'OCASTMethodMetadataAnalyser' }

{ #category : '*Debugging-Utils' }
OCASTMethodMetadataAnalyser >> visitMessageNode: aNode [

	super visitMessageNode: aNode.
	aNode isHaltNode ifTrue: [
		aNode methodNode methodPropertyAt: #containsHalt put: true ]
]
