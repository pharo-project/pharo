accessing-ui
requiredProjectMenu: aMenu
	aMenu addMenuGroup: (MenuGroupPresenter new
		addMenuItem: (MenuItemPresenter new
			name: #'browse project';
			action: [ self browseSelectedProject ]);
		addMenuItem: (MenuItemPresenter new
			name: #'edit version';
			action: [ self projectBoundToSelection editVersion.
					projectList roots: project requiredProjects ]);
		addMenuItem: (MenuItemPresenter new
			name: #'edit repositories';
			action: [ self projectBoundToSelection editRepositories ]);
		yourself).

	^ aMenu