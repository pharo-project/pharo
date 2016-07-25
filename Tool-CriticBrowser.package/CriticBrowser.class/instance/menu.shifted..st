menu
menu: aMenu shifted: aBoolean
	aMenu
		addGroup:[ :group | 
			group addItem: [ :item |
				item 
					name: 'Browse rule' translated;
					action: [ self browseRule ]].
			group addItem: [ :item |
				item 
					name: 'Reapply this rule' translated;
					action: [ self reapplyThisRule ] ] ];
		addGroup:[ :group | 
			group addItem: [ :item |
				item 
					name: 'Ban from selected packages' translated;
					action: [ self addRuleToFalsePositive ]].
			group addItem: [ :item |
				item 
					name: 'Unban from selected packages' translated;
					action: [ self removeRuleToFalsePositive ] ] ].
		
	 ^ aMenu