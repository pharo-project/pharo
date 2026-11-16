Extension { #name : 'MCFileBasedRepository' }

{ #category : '*Metacello-Core' }
MCFileBasedRepository >> baselineOf: aProjectName [
	"The Monticello definitions of BaselineOf<aProjectName> as I hold it now: a directory names no version. nil when I hold none, as baselineOf:atVersion: answers"

	| info |
	info := (self versionInfoFromVersionNamed: 'BaselineOf' , aProjectName) ifNil: [ ^ nil ].
	^ (self versionWithInfo: info ifAbsent: [ ^ nil ]) snapshot definitions
]

{ #category : '*Metacello-Core' }
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
