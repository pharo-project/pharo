building-menu
addMenuEntriesOn: aMenuGroup
	| graphvizGenerator |

	aMenuGroup 
		addMenuItem:
			(MenuItemPresenter new
						name: 'Generate GraphViz file';
						action: [ self ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
									graphvizGenerator := DAPackageCycleGenerator 
										newWith: self cycle 
										named: 'CycleGraph' , self cycle size asString , 'nodes'.
									self saveGraphAndOpenWith: graphvizGenerator ])