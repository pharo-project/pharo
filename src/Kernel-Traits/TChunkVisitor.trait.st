"
I am a trait defining the basic methods for visiting a chunk object.
"
Trait {
	#name : 'TChunkVisitor',
	#category : 'Kernel-Traits-CodeImporter',
	#package : 'Kernel-Traits',
	#tag : 'CodeImporter'
}

{ #category : 'visitor' }
TChunkVisitor >> visitClassChunk: aClassChunk [

	self notYetImplemented
]

{ #category : 'visitor' }
TChunkVisitor >> visitClassCommentChunk: aChunk [

	self visitCodeChunk: aChunk
]

{ #category : 'visitor' }
TChunkVisitor >> visitClassOrganizationChunk: aChunk [

	self visitCodeChunk: aChunk
]

{ #category : 'visitor' }
TChunkVisitor >> visitCodeChunk: aChunk [

	"Base method called from every other visit*"
]

{ #category : 'visitor' }
TChunkVisitor >> visitDoItChunk: aChunk [

	self visitCodeChunk: aChunk
]

{ #category : 'visitor' }
TChunkVisitor >> visitMethodChunk: aChunk [

	self visitCodeChunk: aChunk
]

{ #category : 'visitor' }
TChunkVisitor >> visitStyleChunk: aChunk [

	self visitCodeChunk: aChunk
]
