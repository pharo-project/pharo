Extension { #name : #RBMethodNode }

{ #category : #'*Deprecated90' }
RBMethodNode >> methodComments [
	| methodComments |
	self deprecated: 'no users, will be removed in Pharo10'.
	methodComments := OrderedCollection withAll: self comments.
	arguments do: [:each | methodComments addAll: each comments].
	^methodComments asSortedCollection: [:a :b | a start < b start]
]

{ #category : #'*Deprecated90' }
RBMethodNode >> primitiveSources [
	self deprecated: 'please use the #pragmas directly'.
	^ self pragmas collect: [ :each | self source copyFrom: each first to: each last ]
]
