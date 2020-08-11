Extension { #name : #CmCommandGroupDecorator }

{ #category : #'*NewTools-Debugger' }
CmCommandGroupDecorator >> commandOrGroupNamed: aString ifNone: aBlock [
	^ self decoratedGroup commandOrGroupNamed: aString ifNone: aBlock
]
