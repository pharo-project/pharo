initialization
initializeGroupsWidget
	groupList := (self instantiate: TreeModel)
		autoDeselection: true;
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | | passengerItem myreceiver | 
			myreceiver := receiver.
			myreceiver parentNode ifNotNil: [ myreceiver := myreceiver parentNode ]. "Only add dependencies on top elements"
			passengerItem := transfer passenger item.
			myreceiver content  
				addDependency: ((passengerItem isKindOf: MTDependency) "Ugly test because String>>name adds extra quotes"
					ifTrue: [ passengerItem name ]
					ifFalse: [ passengerItem ]) ];
		whenBuiltDo: [ groupList rootNodeHolder: 
							[ :item | VSGroupNode new content: item ] ];
		yourself.
	
	addGroupButton := (self instantiate: ButtonModel)
		label: nil;
		icon: (Smalltalk ui icons iconNamed: #addIcon);
		enabled: false;
		help: 'Add a new group to this project'.
	
	removeGroupButton  := (self instantiate: ButtonModel)
		label: nil;
		icon: (Smalltalk ui icons iconNamed: #deleteIcon);
		enabled: false;
		help: 'Remove a group from this project'.