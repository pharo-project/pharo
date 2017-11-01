Extension { #name : #MCFileBasedRepository }

{ #category : #'*metacello-mc' }
MCFileBasedRepository >> versionInfoFromVersionNamed: aString [

	| versions |
	versions := self allVersionNames
		select: [ :each | each beginsWith: aString ].
	versions isEmpty ifTrue: [ ^ nil ].
	versions := versions asSortedCollection: [ :a :b |
		([ (a copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) <= 
			([ (b copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) ].
	^ self versionInfoFromFileNamed: versions last , '.mcz'
]
