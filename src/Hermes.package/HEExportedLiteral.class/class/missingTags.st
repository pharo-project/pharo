instance creation
missingTags
	^ self allSubclasses reject: [ :e | e class canUnderstand: #tag ]