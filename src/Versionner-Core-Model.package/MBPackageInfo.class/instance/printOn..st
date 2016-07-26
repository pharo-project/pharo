printing
printOn: aStream
	| wc |
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	(wc := self workingCopy) isNil
		ifTrue: [ 
			aStream
				nextPutAll: self packageName;
				nextPutAll: ' ---' ]
		ifFalse: [ 
			wc ancestry ancestors notEmpty
				ifTrue: [ 
					| loadedPkgName refString |
					loadedPkgName := wc ancestry ancestors first name.
					(refString := self extractPackageSignatureFrom: wc) notEmpty
						ifTrue: [ refString := '[' , refString , ']' ].
					loadedPkgName = self name
						ifTrue: [ aStream nextPutAll: self name ]
						ifFalse: [ 
							(self compareWorkingCopyNamed: loadedPkgName using: #<=)
								ifTrue: [ refString := Text string: refString attributes: TextColor red ].
							aStream
								nextPutAll: self name;
								nextPutAll: refString ] ] ]