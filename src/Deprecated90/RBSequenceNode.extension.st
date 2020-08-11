Extension { #name : #RBSequenceNode }

{ #category : #'*Deprecated90' }
RBSequenceNode >> methodComments [
	| methodComments |
	self deprecated: 'no users, will be removed in Pharo10'.
	methodComments := OrderedCollection withAll: self comments.
	temporaries do: [:each | methodComments addAll: each comments].
	(parent notNil and: [parent isBlock]) 
		ifTrue: [parent arguments do: [:each | methodComments addAll: each comments]].
	^methodComments asSortedCollection: [:a :b | a start < b start]
]
