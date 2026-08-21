Extension { #name : 'PositionableStream' }

{ #category : '*Compression' }
PositionableStream >> asZLibReadStream [

	^ ZLibReadStream on: self collection from: self position + 1 to: self readLimit
]
