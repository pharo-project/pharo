Extension { #name : 'SDLAbstractPlatform' }

{ #category : '*OSWindow-Morphic' }
SDLAbstractPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [

	self subclassResponsibility
]

{ #category : '*OSWindow-Morphic' }
SDLAbstractPlatform >> beforeMainPharoWindowClosed: aOSSDLWindow [

	self subclassResponsibility
]
