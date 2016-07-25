printing
printOn: aStream
	| cv specVersion |
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	super printOn: aStream.
	(cv := self currentVersion) isEmpty
		ifTrue: [ aStream nextPutAll: ' ---' ]
		ifFalse: [ 
			(specVersion := self spec versionOrNil) ~~ nil
				ifTrue: [ 
					aStream nextPutAll: ' ' , specVersion versionString.
					cv asMetacelloVersionNumber = specVersion versionNumber
						ifFalse: [ aStream nextPutAll: ' [' , cv , ']' ] ] ]