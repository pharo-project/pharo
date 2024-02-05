Extension { #name : 'Archive' }

{ #category : '*System-Spec' }
Archive class >> application [

	^ StPharoApplication current
]

{ #category : '*System-Spec' }
Archive class >> openDirectoryDialog [
	"Answer a new instance of <StOpenDirectoryDialog>"

	^ self application openDirectoryDialog
]
