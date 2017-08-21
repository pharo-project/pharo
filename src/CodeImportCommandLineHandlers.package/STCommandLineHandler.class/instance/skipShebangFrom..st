installing
skipShebangFrom: readStream
	"Skip the shebang sequnce #! at the beginning of a bash file"
	
	readStream wrappedStream position = 0
		ifFalse: [  ^ self "#! can only be skipped at the beginning" ].
	
	(readStream wrappedStream next: 2) = '#!'
		ifFalse: [ 
			readStream wrappedStream position: 0.
			^ self ].
	
	"here we found the shebang, so skip the first line"
	^ readStream nextLine.
	