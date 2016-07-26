private
endPC
	^closureOrNil
		ifNil:	[self method endPC]
		ifNotNil: [closureOrNil endPC]