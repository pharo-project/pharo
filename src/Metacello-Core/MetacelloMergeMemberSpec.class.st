Class {
	#name : 'MetacelloMergeMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloMergeMemberSpec >> applyTo: aCollection [

	aCollection
		detect: [ :aSpec | aSpec name = self spec name ]
		ifFound: [ :aSpec |
				aCollection
					remove: aSpec;
					add: (aSpec mergeSpec: self spec) ]
		ifNone: [ aCollection add: self spec ]
]
