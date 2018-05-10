Extension { #name : #String }

{ #category : #'*Alien-Core' }
String >> asAlien [
	| bs |
	^(Alien newC: (bs := self byteSize) + 1)
		replaceFrom: 1 to: bs with: self startingAt: 1
]
