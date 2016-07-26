accessing
selectedNodeList
	^ self selectedNodePathList 
		ifNil: [#()]
		ifNotNil: [:l | l collect: [:path | path last]]