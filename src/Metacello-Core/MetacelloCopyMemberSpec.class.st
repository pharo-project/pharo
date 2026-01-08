Class {
	#name : 'MetacelloCopyMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloCopyMemberSpec >> applyTo: aCollection [

	aCollection
		detect: [ :aSpec | aSpec name = self spec originalName ]
		ifFound: [ :aSpec |
				aSpec aboutToCopy.
				aCollection add: (aSpec copy mergeSpec: self spec copy) ]
		ifNone: [ self error: 'Source spec named ' , self spec originalName printString , ' not found' ]
]
