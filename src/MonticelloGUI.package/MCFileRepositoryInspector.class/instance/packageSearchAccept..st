morphic ui
packageSearchAccept: string
	| aString |

	aString := string ifNil: [ '' ].
	packagePattern = aString asLowercase ifTrue: [ ^ self ].
	packagePattern :=  aString asLowercase.

   	self packageListUpdate.