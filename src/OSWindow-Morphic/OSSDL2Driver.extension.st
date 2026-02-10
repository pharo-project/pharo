Extension { #name : 'OSSDL2Driver' }

{ #category : '*OSWindow-Morphic' }
OSSDL2Driver >> afterMainPharoWindowCreated: osWindow [

	OSPlatform current sdlPlatform
		afterMainPharoWindowCreated: osWindow
]

{ #category : '*OSWindow-Morphic' }
OSSDL2Driver >> beforeMainPharoWindowClosed: osWindow [

	OSPlatform current sdlPlatform
		beforeMainPharoWindowClosed: osWindow
]
