Extension { #name : #Duration }

{ #category : #'*Network-Mail' }
Duration >> asEmailTimeOffsetString [
	"Format per RFC5322  e.g.  '-0500'"

	^ String streamContents: [ :str | 
			str
				nextPut: (self positive ifTrue: [ $+ ] ifFalse: [ $- ]);
				nextPutAll: self hours abs asTwoCharacterString;
				nextPutAll: self minutes asTwoCharacterString ].
]
