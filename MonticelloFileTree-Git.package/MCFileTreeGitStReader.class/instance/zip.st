accessing
zip
	"Get the zip based on the commitID."

	zip isNil
		ifTrue: [ 
			zip := ZipArchive new.
			zip
				readFrom:
					(self
						gitCommand:
							{'archive'.
							'--format=zip'.
							(commitID hex).
							packageDirectory}
						in: stream) asByteArray readStream ].
	^ zip