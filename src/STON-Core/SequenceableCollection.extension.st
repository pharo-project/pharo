Extension { #name : 'SequenceableCollection' }

{ #category : '*STON-Core' }
SequenceableCollection class >> fromSton: stonReader [
	"Overwritten to use #streamContents: and #nextPut:"

	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
]

{ #category : '*STON-Core' }
SequenceableCollection >> stonOn: stonWriter [
	"Instances of STON listClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a list representation. "

	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
]
