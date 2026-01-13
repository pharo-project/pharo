Class {
	#name : 'MetacelloAddMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloAddMemberSpec >> applyToMap: map [

	map at: self name put: self spec
]
