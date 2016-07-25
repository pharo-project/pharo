accessing
command
	"return a bash-like lauch command for the vm including all arguments up to the image name "
	^ String streamContents: [ :s|
		s nextPutAll: (self optionAt: 0).
		Smalltalk commandLine options 
			do: [ :argument|
				(argument endsWith: '.image')
					ifTrue: [ ^ s contents ].
				
				s space; nextPutAll: argument]]