initialize-release
beForSmalltalkCodeInClass: aClass
	self editingMode: (RubSmalltalkCodeMode new
		classOrMetaClass: aClass;
		yourself)