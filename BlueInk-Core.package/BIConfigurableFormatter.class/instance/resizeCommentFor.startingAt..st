utility
resizeCommentFor: aComment startingAt: anIndex
	codeStream nextPut: $".
	"Hack to keep the right comment in pop up like cmd n, cmd m..."
	self maxLineLength < 50
		ifTrue: [ codeStream nextPutAll: aComment contents ]
		ifFalse:
			[ 
			aComment contents
				ifNotEmpty:
					[ 
					(anIndex = 0 or: [ anIndex >= self maxLineLength ])
						ifTrue: [ self resizeStringDependingOnWindowSizeFor: aComment ]
						ifFalse: [ self resizeComment: aComment withFirstLineShorterOf: anIndex ] ] ].
	codeStream nextPut: $"