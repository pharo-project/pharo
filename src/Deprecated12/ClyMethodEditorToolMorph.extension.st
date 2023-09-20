Extension { #name : 'ClyMethodEditorToolMorph' }

{ #category : '*Deprecated12' }
ClyMethodEditorToolMorph >> methodTags [

	self deprecated: 'Use #methodProtocol instead but note that it returns a protocol name and not a collection.'.
	^ { self methodProtocol }
]

{ #category : '*Deprecated12' }
ClyMethodEditorToolMorph >> methodTags: tagsArray [

	self deprecated: 'Use #methodProtocol: instead but be sure to give a protocol name and not a collection.'.
	self methodProtocol: (tagsArray
			 ifNotEmpty: [ tagsArray anyOne ]
			 ifEmpty: [ nil ])
]
