printing
configMethodBodyOn: aStream hasName: hasName cascading: cascading indent: indent

	| hasCascading hasRequires hasIncludes hasAnswers |
	hasCascading := cascading.
	hasRequires := self requires isEmpty not.
	hasIncludes := self includes isEmpty not.
	hasAnswers := self answers isEmpty not.
	hasRequires
		ifTrue: [ 
			hasName | hasIncludes | hasAnswers | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'requires: #('.
			self requires do: [:str | aStream nextPutAll: str printString, ' ' ].
			hasIncludes | hasAnswers | hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].
	hasIncludes
		ifTrue: [ 
			hasName | hasRequires | hasAnswers | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'includes: #('.
			self includes do: [:str | aStream nextPutAll: str printString, ' ' ].
			hasAnswers | hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].
	hasAnswers
		ifTrue: [ 
			hasName | hasRequires | hasIncludes | hasCascading
				ifTrue: [ aStream cr; tab: indent ].
			aStream nextPutAll: 'supplyingAnswers: #( '.
			self answers do: [:ar | 
				aStream nextPutAll: '#( '.
				ar do: [:val | 
					(val isString or: [ val isNumber or: [ val isSymbol or: [ val isCharacter ]]])
						ifTrue: [  aStream nextPutAll: val printString, ' ' ].
					val == true
						ifTrue: [  aStream nextPutAll: 'true ' ].
					val == false
						ifTrue: [  aStream nextPutAll: 'false ' ]].
				aStream nextPutAll: ') ' ].
			hasCascading
				ifTrue: [ aStream nextPutAll: ');' ]
				ifFalse: [ aStream nextPut: $) ]].