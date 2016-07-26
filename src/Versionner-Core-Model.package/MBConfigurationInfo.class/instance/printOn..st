printing
printOn: aStream
	| cv nameString wc refString branchString |
	[self name
		ifNil: [ 
			aStream nextPutAll: ''.
			^ self ].
	nameString := self name configurationBaseName.
	(refString := self extractPackageSignatureFrom: (wc := self workingCopy)) notEmpty
		ifTrue: [ refString := '(' , refString , ')' ].
	(branchString := self branch) notEmpty
		ifTrue: [ nameString := nameString, '.', branchString ].
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	aStream nextPutAll: nameString.
	(cv := self currentVersion) notNil
		ifTrue: [ aStream nextPutAll: ' ' , cv versionString ].
	aStream nextPutAll: refString.] on:Error do:[].