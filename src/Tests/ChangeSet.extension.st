Extension { #name : #ChangeSet }

{ #category : #'*Tests' }
ChangeSet class >> newTestChangeSet [

	^ self newChangeSet: self testName
]

{ #category : #'*Tests' }
ChangeSet class >> testName [
	^ self uniqueNameLike: 'Test'
]
