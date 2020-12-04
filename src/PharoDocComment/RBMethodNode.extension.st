Extension { #name : #RBMethodNode }

{ #category : #'*PharoDocComment' }
RBMethodNode >> pharoDocCommentNodes [
	"Return a collection of pharo comment nodes: expressions following the pattern"
	"true >>> true"
	
	^ self comments flatCollect: [:each | each pharoDocCommentNodes]
]
