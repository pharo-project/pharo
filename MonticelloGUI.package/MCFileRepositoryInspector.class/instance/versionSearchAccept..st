morphic ui
versionSearchAccept: string
	| aString |

	aString := string ifNil: [ '' ].
	versionPattern = aString asLowercase ifTrue: [ ^ self ].
	versionPattern := aString asLowercase.

	versionProcess ifNotNil: [ versionProcess terminate ].
	versionProcess := [ self changed: #versionList ] fork.