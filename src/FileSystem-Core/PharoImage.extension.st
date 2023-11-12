Extension { #name : 'PharoImage' }

{ #category : '*FileSystem-Core' }
PharoImage >> changesFile [
	"Answer the file containing the current changes."

	^ FileLocator changes
]

{ #category : '*FileSystem-Core' }
PharoImage >> imageFile [

	^ FileLocator image
]
