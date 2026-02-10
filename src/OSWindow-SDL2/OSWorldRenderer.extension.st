Extension { #name : 'OSWorldRenderer' }

{ #category : '*OSWindow-SDL2' }
OSWorldRenderer class >> restart [

	| oldWindows |
	UIManager default deactivate.

	"We need to keep all open windows, after the restart if they are still valid we register them back. If we don't do this the events for these windows are never delivered."
	oldWindows := OSSDL2Driver allWindows.

	self shutDown: true.
	OSSDL2Driver shutDown: true.

	self startUp: true.
	UIManager default activate.

	oldWindows 
	 select: [ :e | e sdl2Window isNotNil ]
	 thenDo: [ :e | OSSDL2Driver current registerWindow: e ].

	InformativeNotification signal: self class name , ' restarted'
]
