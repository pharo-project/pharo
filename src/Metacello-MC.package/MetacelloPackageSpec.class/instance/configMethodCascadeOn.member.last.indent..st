printing
configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent

	aMember methodUpdateSelector  == #remove:
		ifTrue: [ aStream  nextPutAll: 'removePackage: ', self name printString ]
		ifFalse: [ self configShortCutMethodBodyOn: aStream member: aMember indent: indent ].
	lastCascade
		ifTrue: [ aStream nextPut: $. ]
		ifFalse: [ aStream nextPut: $;; cr ]