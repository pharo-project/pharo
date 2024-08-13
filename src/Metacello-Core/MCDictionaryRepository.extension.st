Extension { #name : 'MCDictionaryRepository' }

{ #category : '*Metacello-Core' }
MCDictionaryRepository >> asRepositorySpecFor: aMetacelloMCProject [

	| desc |
	desc := self description.
	desc ifNil: [ desc := 'dictionary://Metacello_Dictionary' ].
	^(aMetacelloMCProject repositorySpec)
		description:  desc;
	 	type: 'dictionary';
		yourself
]

{ #category : '*Metacello-Core' }
MCDictionaryRepository class >> createRepositoryFromSpec: aRepositorySpec [

	| globalName |
	globalName := (aRepositorySpec description copyFrom: 'dictionary://' size + 1 to: aRepositorySpec description size) asSymbol.

	^ Smalltalk at: globalName ifAbsentPut: [
		  self new
			  description: aRepositorySpec description;
			  yourself ]
]

{ #category : '*Metacello-Core' }
MCDictionaryRepository class >> isAvailableFor: type [
	^ type = 'dictionary'
]

{ #category : '*Metacello-Core' }
MCDictionaryRepository >> versionInfoFromVersionNamed: aString [

	| versions |
	versions := self dictionary values select: [:version | version info name beginsWith: aString ].
	versions isEmpty ifTrue: [ ^ nil ].
	versions := versions asSortedCollection: [ :a :b |
		([ (a info name copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) <= 
			([ (b info name copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) ].
	^ versions last info
]
