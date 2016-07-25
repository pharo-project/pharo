installing
installSourceFiles
	"Install the previously loaded source files"

	sourceFiles ifNil: [ ^ self ].
	[ sourceFiles do: [ :reference | 
		self installSourceFile: reference ]
	] ensure: [ sourceFiles := nil ].
	