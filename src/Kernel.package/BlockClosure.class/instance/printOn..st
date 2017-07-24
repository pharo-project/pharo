printing
printOn: aStream
	Smalltalk globals
		at: #OpalCompiler
		ifPresent: [ :opalClass |
			opalClass isActive
				ifTrue: [ ^ aStream nextPutAll:  self sourceNode formattedCode] ].
	^ 'aBlockClosure([...])'