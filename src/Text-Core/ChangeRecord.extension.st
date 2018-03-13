Extension { #name : #ChangeRecord }

{ #category : #'*Text-Core' }
ChangeRecord >> text [
	| text |
	^ file ifNil: ['']
		ifNotNil: [
			file tryOpenReadOnly: true.
			file position: position.
			text := file nextChunk asText.
			file close.
			text]
]
