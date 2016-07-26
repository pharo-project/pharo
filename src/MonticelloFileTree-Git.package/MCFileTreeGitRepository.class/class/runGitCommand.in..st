accessing
runGitCommand: anArrayOfStrings in: aDirectory
	"Dispatch depending on the platform."

	UIManager default
		informUserDuring: [ :bar | 
			bar label: self gitCommand , ' ' , anArrayOfStrings first.
			Smalltalk platform isWin32
				ifTrue:
					[ ^ self runProcessWrapperGitCommand: anArrayOfStrings in: aDirectory ]
				ifFalse: [ ((Smalltalk at: #OSSUnixSubprocess ifAbsent: [  ])
						ifNil: [ false ]
						ifNotNil: [ :g | g isClass ])
						ifTrue: [ ^ self runOSSubprocessGitCommand: anArrayOfStrings in: aDirectory ]
						ifFalse: [ ^ self runOSProcessGitCommand: anArrayOfStrings in: aDirectory ] ] ]