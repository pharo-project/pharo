window creation and deletion
initializeWindowMap

	WindowMap ifNil: [ 
		WindowMap := WeakValueDictionary new. 
	].