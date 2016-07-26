callbacks
menu: aMenuMorph 
	| subMenus targetMenuMorph subMenu allActions |
	subMenus := Dictionary new.
	allActions := self allMenuActions.
	allActions do: [ :action |
		targetMenuMorph := action category notNil 
			ifTrue: [ subMenus at: action category ifAbsentPut: [ 
					subMenu :=  MenuMorph new.
					aMenuMorph add: action category subMenu: subMenu.
					subMenu ] ]
			ifFalse: [ aMenuMorph ].
		(action separatorBefore and: [ allActions first ~= action ]) ifTrue: [ targetMenuMorph addLine ].
		(targetMenuMorph 
			add: action title
			target: self
			selector: #executeMenuAction:
			argument: action)
			enabled: (action isEnabledOn: self glamourPresentation);
			icon:  action icon;
			keyText: (action hasShortcut 
				ifTrue: [ action shortcutAsString ]
				ifFalse: [ nil ]).
		(action separatorAfter and: [ allActions last ~= action ]) ifTrue: [ targetMenuMorph addLine ] ].
	^ aMenuMorph