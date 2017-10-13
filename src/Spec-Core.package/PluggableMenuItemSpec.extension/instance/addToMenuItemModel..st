*Spec-Core
addToMenuItemModel: aMenuItemModel
	| en newSubMenu |
	en := self enabledBlock ifNil: [ self enabled ].
	newSubMenu := self subMenu
		ifNotNil: [ :sub | MenuPresenter new fromSpec: sub ].
	aMenuItemModel
		name: self label;
		state: self checked;
		enabled: en;
		icon: self icon;
		shortcut: self keyText;
		subMenu: newSubMenu;
		action: self action