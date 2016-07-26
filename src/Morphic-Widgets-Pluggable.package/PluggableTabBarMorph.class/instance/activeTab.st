private - access
activeTab
	activeTab ifNil: [
		self tabs size > 0 ifTrue: [
			activeTab := self tabs first key.
			activeTab active: true]].
	^ activeTab 