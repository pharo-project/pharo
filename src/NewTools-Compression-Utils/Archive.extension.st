Extension { #name : 'Archive' }

{ #category : '*NewTools-Compression-Utils' }
Archive class >> application [

	^ StPharoApplication current
]

{ #category : '*NewTools-Compression-Utils' }
Archive class >> openDirectoryDialog [
	"Answer a new instance of <StOpenDirectoryDialog>"

	^ self application openDirectoryDialog
]
