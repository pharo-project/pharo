Extension { #name : 'MCFileTreeRepository' }

{ #category : '*Metacello-MC' }
MCFileTreeRepository class >> isAvailableFor: type [
	^ type = 'filetree'
]
