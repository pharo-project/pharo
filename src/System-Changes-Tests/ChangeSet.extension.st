Extension { #name : 'ChangeSet' }

{ #category : '*System-Changes-Tests' }
ChangeSet class >> newTestChangeSet [

	^ self newChangeSet: self testName
]

{ #category : '*System-Changes-Tests' }
ChangeSet class >> testName [
	^ self uniqueNameLike: 'Test'
]
