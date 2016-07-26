updating
updateFromSelection: aSelection
	aSelection selectedNodePathList do: [:path | self selectNodePath: path].
	self scrollSelectionIntoView