protocol
menuGroups
	^ self model menuGroups reject: [ :e | e isEmpty ]