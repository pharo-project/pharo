Extension { #name : #ChangeSet }

{ #category : #'*Tests' }
ChangeSet class >> newTestChangeSet [

	^ self basicNewChangeSet: self testName
]

{ #category : #'*Tests' }
ChangeSet class >> testName [
	^ self uniqueNameLike: 'Test'
]
