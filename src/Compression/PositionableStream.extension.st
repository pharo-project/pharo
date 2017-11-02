Extension { #name : #PositionableStream }

{ #category : #'*Compression' }
PositionableStream >> asZLibReadStream [
	^ZLibReadStream on: collection from: position+1 to: readLimit
]
