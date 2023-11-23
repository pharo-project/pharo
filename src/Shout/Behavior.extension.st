Extension { #name : 'Behavior' }

{ #category : '*Shout' }
Behavior >> syntaxHighlighter [
	"Answer a compiler appropriate for source methods of this class."

	^ SHRBTextStyler new
		classOrMetaClass: self;
		yourself
]
