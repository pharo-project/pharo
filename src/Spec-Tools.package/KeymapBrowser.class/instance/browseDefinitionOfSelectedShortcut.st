private
browseDefinitionOfSelectedShortcut
	| selection action class selector |
	selection := self kmTable selectedItem.
	selection ifNil: [ ^ self ].
	action := selection realValue keymap action.
	action ifNil: [ ^ self ].
	class := action method methodClass.
	selector := action method selector.
	Smalltalk tools browser openOnClass: class selector: selector highlight: selection shortcutName