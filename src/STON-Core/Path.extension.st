Extension { #name : 'Path' }

{ #category : '*STON-Core' }
Path class >> fromSton: stonReader [
	| elements |
	elements := Array streamContents: [ :out |
		stonReader parseListDo: [ :each | out nextPut: each ] ].
	^ self withAll: elements
]

{ #category : '*STON-Core' }
Path >> stonOn: stonWriter [
	stonWriter
		writeObject: self
		streamShortList: [ :listWriter |
			self do: [ :each | listWriter add: each ] ]
]
