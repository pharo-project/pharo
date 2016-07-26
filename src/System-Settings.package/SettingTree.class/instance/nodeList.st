accessing
nodeList
	| builder |
	^ nodeList 
		ifNil: [
			builder := SettingTreeBuilder new.
			self pragmasDo: [:p | builder buildPragma: p].
			nodeList := builder nodeList.
			nodeList do: [:n | n model: self].
			self checkForUnknownParent.
			self checkForUnknownTarget.
			self nodeList]