menu
loadMenuFor: mbInfo in: aMenu 
	| commands menuGroup |
	
	"dynamic menu"
	commands := mbInfo commands collect: [ :commandClass | 
		commandClass 
			target: mbInfo 
			for: (configurationList selectedItem) ].
	commands := commands sorted: [:a :b | a order <= b order].
	
	menuGroup := MenuGroupModel new.
	commands do: [ :command |
		menuGroup addMenuItem: 
			(MenuItemModel new
				name: command title;
				action: [ self unregister.
						 command execute.
						 self register ]) ].
	menuGroup buildWithSpec do: [ :eachMenuItem |
		aMenu addMenuItem: eachMenuItem ].
	^aMenu