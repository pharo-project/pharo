Extension { #name : #SequenceableCollection }

{ #category : #'*ston-core' }
SequenceableCollection class >> fromSton: stonReader [
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
]

{ #category : #'*ston-core' }
SequenceableCollection >> stonOn: stonWriter [
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
]
