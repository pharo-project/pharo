Extension { #name : 'SmalltalkImage' }

{ #category : '*UIManager' }
SmalltalkImage >> getFileNameFromUser [

	| givenName strippedName imageFile changesFile |
	givenName := UIManager default
		request: 'New File Name?'
		initialAnswer: (self imageFile basename copyUpToLast:  Path extensionDelimiter).
	strippedName := self stripImageExtensionFrom: givenName.
	strippedName isEmptyOrNil ifTrue: [^nil].
	imageFile := self fileForImageNamed: strippedName.
	changesFile := self fileForChangesNamed: strippedName.
	((imageFile exists or: [changesFile exists]) and:
		[(self confirm: ('{1} already exists. Overwrite?' format: {strippedName})) not])
			ifTrue: [^nil].
	^strippedName
]

{ #category : '*UIManager' }
SmalltalkImage >> saveAs [
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self saveAs: self getFileNameFromUser
]
