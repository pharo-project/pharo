installing
skipShebangFrom: readStream
	"Skip the shebang sequnce #! at the beginning of a bash file"
	
	readStream position = 0
		ifFalse: [  ^ self "#! can only be skipped at the beginning" ].
		readStream position: 0.
	
	(readStream next: 2) = '#!'
		ifFalse: [ 
			readStream position: 0.
			^ self ].
	
	"here we found the shebang, so skip the first line"
	^ readStream nextLine.
	