Extension { #name : 'MorphExtension' }

{ #category : '*Morphic-Halos' }
MorphExtension >> inspectElement [
	"Create and schedule an Inspector on the otherProperties and the
	named properties."
	| key obj |
	key := self morphicUIManager
				chooseFrom: self sortedPropertyNames
				values: self sortedPropertyNames
				title: 'Inspect which property?' translated.
	key
		ifNil: [^ self].
	obj := otherProperties
				at: key
				ifAbsent: ['nOT a vALuE'].
	obj = 'nOT a vALuE'
		ifTrue: [(self perform: key) inspect
			"named properties"]
		ifFalse: [obj inspect]
]
