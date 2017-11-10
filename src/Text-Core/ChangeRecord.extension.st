Extension { #name : #ChangeRecord }

{ #category : #'*Text-Core' }
ChangeRecord >> text [
	| text |
	^ file ifNil: ['']
		ifNotNil: [
			file openReadOnly.
			file position: position.
			text := file nextChunkText.
			file close.
			text]
]
