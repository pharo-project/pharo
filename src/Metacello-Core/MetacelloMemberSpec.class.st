Class {
	#name : 'MetacelloMemberSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'name',
		'spec'
	],
	#category : 'Metacello-Core-Members',
	#package : 'Metacello-Core',
	#tag : 'Members'
}

{ #category : 'adding' }
MetacelloMemberSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : 'adding' }
MetacelloMemberSpec >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : 'actions' }
MetacelloMemberSpec >> applyAdd: addBlock copy: copyBlock merge: mergeBlock remove: removeBlock [

	self subclassResponsibility
]

{ #category : 'actions' }
MetacelloMemberSpec >> applyToList: aListSpec [

	self subclassResponsibility
]

{ #category : 'printing' }
MetacelloMemberSpec >> configMethodCascadeOn: aStream last: lastCascade indent: indent [

	self spec 
		configMethodCascadeOn: aStream 
		member: self 
		last: lastCascade 
		indent: indent
]

{ #category : 'printing' }
MetacelloMemberSpec >> configMethodOn: aStream indent: indent [

	aStream nextPutAll: self class name asString, ' member: ('.
	self spec configMethodOn: aStream indent: indent.
	aStream nextPutAll: ')'.
]

{ #category : 'merging' }
MetacelloMemberSpec >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : 'merging' }
MetacelloMemberSpec >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : 'accessing' }
MetacelloMemberSpec >> methodUpdateSelector [

	^self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloMemberSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloMemberSpec >> name: aString [

	name := aString
]

{ #category : 'removing' }
MetacelloMemberSpec >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: self
]

{ #category : 'removing' }
MetacelloMemberSpec >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: self
]

{ #category : 'accessing' }
MetacelloMemberSpec >> sourceName [

	^self name
]

{ #category : 'accessing' }
MetacelloMemberSpec >> spec [

	^spec
]

{ #category : 'accessing' }
MetacelloMemberSpec >> spec: aMetacelloSpec [

	spec := aMetacelloSpec
]
