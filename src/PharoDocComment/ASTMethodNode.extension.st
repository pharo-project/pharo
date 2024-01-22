Extension { #name : 'ASTMethodNode' }

{ #category : '*PharoDocComment' }
ASTMethodNode >> pharoDocCommentNodes [
	"Return a collection of pharo comment nodes: expressions following the pattern"
	"true >>> true"

	^ self comments flatCollect: [:each | each pharoDocCommentNodes]
]
