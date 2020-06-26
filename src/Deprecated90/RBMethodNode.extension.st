Extension { #name : #RBMethodNode }

{ #category : #'*Deprecated90' }
RBMethodNode >> primitiveSources [
	self deprecated: 'please use the #pragmas directly'.
	^ self pragmas collect: [ :each | self source copyFrom: each first to: each last ]
]
