Extension { #name : #RBMethodNode }

{ #category : #'*PharoDocComment' }
RBMethodNode >> pharoDocCommentNodes [
	"Return a collection of pharo comment nodes: expressions following the pattern expr1 >>> expr2"
	
	^ self comments flatCollect: [:each | each pharoDocCommentNodes]
]
