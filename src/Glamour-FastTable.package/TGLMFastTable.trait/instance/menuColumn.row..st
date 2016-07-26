menu
menuColumn: column row: rowIndex
	| menu subMenus targetMenuMorph subMenu |
	
	menu := UIManager default newMenuIn: self table for: self.
	subMenus := Dictionary new.
	self allMenuActions do: [ :action |
		targetMenuMorph := action category notNil 
			ifTrue: [ subMenus at: action category ifAbsentPut: [ 
					subMenu :=  MenuMorph new.
					menu add: action category subMenu: subMenu.
					subMenu ] ]
			ifFalse: [ menu ].
		(targetMenuMorph 
			add: action title
			target: self
			selector: #executeMenuAction:
			argument: action)
			icon:  action icon;
			keyText: (action hasShortcut 
				ifTrue: [ action shortcutAsString ]
				ifFalse: [ nil ]) ].
	^ menu