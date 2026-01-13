Class {
	#name : 'MetacelloRemoveMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloRemoveMemberSpec >> applyToMap: map [

	map removeKey: self name ifAbsent: [ ]
]
