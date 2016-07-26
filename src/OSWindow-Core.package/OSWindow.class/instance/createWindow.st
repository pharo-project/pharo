private
createWindow
	handle ifNotNil: [ self error: 'The window is already created.' ].
	
	handle := initialAttributes createWindowHandleFor: self.
	handle isValid 
		ifTrue: [ FFIExternalResourceManager addResource: self data: handle ]