Class {
	#name : 'MetacelloMergeMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloMergeMemberSpec >> applyToMap: map [

	map
		at: self name
		ifPresent: [ :aSpec | map at: self name put: (aSpec mergeSpec: self spec) ]
		ifAbsent: [ map at: self name put: self spec copy ]
]
