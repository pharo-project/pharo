Class {
	#name : 'MetacelloAddMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloAddMemberSpec >> applyTo: aCollection [

	aCollection
		detect: [ :aSpec | aSpec name = self spec name ]
		ifFound: [ :aSpec | aCollection remove: aSpec ].
	aCollection add: self spec
]
