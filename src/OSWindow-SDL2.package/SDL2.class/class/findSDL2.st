native
findSDL2
	"In Linux, try to use the linked version."
	OSSDL2Driver hasPlugin.
	
	"Look for SDL2 using different names."
	#(0							"Static"
	SDL2
	'libSDL2-2.0.0.dylib'		"Mac"
	'libSDL2-2.0.so.0'			"Linux 1"
	'libSDL2-2.0.so.0.2.1'	"Linux 2"
	'SDL2.dll'					"Windows"
	) do: [ :eachName | 
		[ (self checkLibraryName: eachName) ifTrue: [ ^ eachName ] ] 
		ifError: [nil] ].
	self error: 'Failed to find SDL2 library.'