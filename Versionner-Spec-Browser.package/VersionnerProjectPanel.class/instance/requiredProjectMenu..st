accessing-ui
requiredProjectMenu: aMenu
	aMenu addMenuGroup: (MenuGroupModel new
		addMenuItem: (MenuItemModel new
			name: #'browse project';
			action: [ self browseSelectedProject ]);
		addMenuItem: (MenuItemModel new
			name: #'edit version';
			action: [ self projectBoundToSelection editVersion.
					projectList roots: project requiredProjects ]);
		addMenuItem: (MenuItemModel new
			name: #'edit repositories';
			action: [ self projectBoundToSelection editRepositories ]);
		yourself).

	^ aMenu