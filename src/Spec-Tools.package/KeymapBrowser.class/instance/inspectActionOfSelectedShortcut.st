private
inspectActionOfSelectedShortcut
	| selection action   |
	selection := self kmTable selectedItem.
	selection ifNil: [ ^ self ].
	action := selection realValue keymap.
	action ifNil: [ ^ self ].
	action inspect