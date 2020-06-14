Extension { #name : #RBParseTreeSearcher }

{ #category : #'*RewriteTool' }
RBParseTreeSearcher >> replaceWith [
	^ searches first replaceWith
]

{ #category : #'*RewriteTool' }
RBParseTreeSearcher >> searchFor [
	^ searches first searchFor
]
