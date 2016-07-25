menu
detachSubMenu: evt
	| possibleTargets item subMenu index |
	possibleTargets := self items select:[ :any | any hasSubMenu].
	possibleTargets isEmpty ifTrue: [ ^self ].
	
	index := UIManager default  
			chooseFrom: (possibleTargets collect:[:t| t contents asString]) 
			title: 'Which menu?' translated.
	index = 0 ifTrue:[^self].
	
	item := possibleTargets at: index.
	subMenu := item subMenu.
	subMenu ifNotNil: [
		item subMenu: nil.
		item delete.
		subMenu stayUp: true.
		subMenu popUpOwner: nil.
		subMenu addTitle: item contents.
		evt hand attachMorph: subMenu
	].
