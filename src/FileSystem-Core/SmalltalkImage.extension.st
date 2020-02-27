Extension { #name : #SmalltalkImage }

{ #category : #'*FileSystem-Core' }
SmalltalkImage >> changesFile [
	"Answer the file containing the current changes."

	^ FileLocator changes

]

{ #category : #'*FileSystem-Core' }
SmalltalkImage >> imageFile [

	^ FileLocator image
]
