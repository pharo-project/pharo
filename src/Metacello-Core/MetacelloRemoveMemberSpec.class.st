Class {
	#name : 'MetacelloRemoveMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloRemoveMemberSpec >> applyTo: aCollection [

	aCollection
		detect: [ :aSpec | aSpec name = self spec name ]
		ifFound: [ :aSpec | aCollection remove: aSpec ]
]
