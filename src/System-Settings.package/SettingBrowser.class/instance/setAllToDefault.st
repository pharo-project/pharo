menu
setAllToDefault
	(self confirm: 'Set all to default value?' translated) ifTrue: [
		self treeHolder nodeList do: [:node | node item hasDefault ifTrue: [node item setToDefault]].
		self updateList]