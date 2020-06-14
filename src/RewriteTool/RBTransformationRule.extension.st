Extension { #name : #RBTransformationRule }

{ #category : #'*RewriteTool' }
RBTransformationRule >> input [
	^ String empty
]

{ #category : #'*RewriteTool' }
RBTransformationRule >> output [
	^ String empty
]

{ #category : #'*RewriteTool' }
RBTransformationRule >> replaceWith [
	^ rewriteRule replaceWith formattedCode
]

{ #category : #'*RewriteTool' }
RBTransformationRule >> searchFor [
	^ rewriteRule searchFor formattedCode
]
