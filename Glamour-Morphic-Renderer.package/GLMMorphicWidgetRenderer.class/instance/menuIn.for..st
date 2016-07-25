private
menuIn: aMenuMorph for: aPresentation
	| subMenus targetMenuMorph subMenu |
	subMenus := Dictionary new.
	(aPresentation allActions select: [:each | each isMenu ]) do: [ :action |
		targetMenuMorph := action category notNil 
			ifTrue: [ subMenus at: action category ifAbsentPut: [ 
					subMenu :=  MenuMorph new.
					aMenuMorph add: action category subMenu: subMenu.
					subMenu ] ]
			ifFalse: [ aMenuMorph ].
		(targetMenuMorph 
			add: action title
			target: action 
			selector: #morphicActOn:
			argument: aPresentation)
			enabled: (action isEnabledOn: aPresentation);
			icon:  action icon;
			keyText: (action hasShortcut 
				ifTrue: [ action shortcutAsString ]
				ifFalse: [ nil ]) ].
	^ aMenuMorph