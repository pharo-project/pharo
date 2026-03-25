Extension { #name : 'WinPlatform' }

{ #category : '*Files' }
WinPlatform >> createStdioFileFor: moniker [
	"I am added by System-Time because I need the date to create the file and it is not available in the minimal image."

	^ [
		  [ Stdio createWriteStreamBlock cull: moniker asString ]
			  on: CannotDeleteFileException
			  do: [ "HACK: if the image is opened a second time windows barks about the already opened locked file"
				| candidateName candidateIndex |
				candidateIndex := 0.
				[
					candidateIndex := candidateIndex + 1.
					candidateName := moniker asString , '_' , candidateIndex asString.
					(File named: candidateName) exists] whileTrue.
				Stdio createWriteStreamBlock cull: candidateName ] ]
		  on: FileException
		  do: [ NullStream new ]
]
