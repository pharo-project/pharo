selection change
updateSelectionFromModel
	(self listModel selection) 
		ifNotNil: [:selHolder | 	
				self isSelectionUpdateFromView
					ifTrue: [
						self listModel selectionChanged.
						self selectionChanged]
					ifFalse: [selHolder updateView: client forModel: self listModel]].
