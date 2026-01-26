Extension { #name : 'WinPlatform' }

{ #category : '*System-Time' }
WinPlatform >> createStdioFileFor: moniker [
	"I am added by System-Time because I need the date to create the file and it is not available in the minimal image."

	^ [
		  [ Stdio createWriteStreamBlock cull: moniker asString ]
			  on: CannotDeleteFileException
			  do: [ "HACK: if the image is opened a second time windows barks about the already opened locked file"
				  Stdio createWriteStreamBlock cull: moniker asString , '_' , DateAndTime now asFileNameCompatibleString ] ]
		  on: FileException
		  do: [ NullStream new ]
]
