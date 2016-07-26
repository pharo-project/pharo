action
interactWithSelectedItem

	self selection
		ifNotNil: [: sel | sel submorphs do: [:each | each update: #interact ]]