private
registerShortcut: newShortcut
	| receiver |
	receiver := self window.
	(receiver isNil or: [ newShortcut isNil ]) ifTrue: [ ^ self ].

	receiver model
		bindKeyCombination: newShortcut 
		toAction: [ self performAction ]