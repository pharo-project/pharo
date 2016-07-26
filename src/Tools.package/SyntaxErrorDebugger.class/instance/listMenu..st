menu
listMenu: aMenu
	^aMenu addList: {
		{'Proceed' . #proceed}.
		{'Debug calling process' . #debug}.
		{'Browse full translated' . #browseMethodFull}}.