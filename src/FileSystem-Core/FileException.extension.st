Extension { #name : 'FileException' }

{ #category : '*FileSystem-Core' }
FileException >> fileName [
	"If the entity is a string we return it. Else we ask it to the file reference."

	^ entity isString
		  ifTrue: [ entity ]
		  ifFalse: [ entity asPath pathString ]
]
