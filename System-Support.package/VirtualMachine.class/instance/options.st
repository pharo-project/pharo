accessing
options
	"return an array with all the options passed to the VM (up to the image) "
	^ Array streamContents: [ :s|
		Smalltalk commandLine options 
			do: [ :argument|
				(argument endsWith: '.image')
					ifTrue: [ ^ s contents ].
				
				s nextPut: argument ]]