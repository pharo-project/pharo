menus behavior
restrictedBrowsePackages: selections
	^ super restrictedBrowsePackages: (selections collect: [ :each | each asNautilusSelection item ])