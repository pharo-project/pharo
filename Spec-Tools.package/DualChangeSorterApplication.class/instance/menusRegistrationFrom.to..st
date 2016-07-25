menu
menusRegistrationFrom: src to: dest
	
	src changesMenu: [:menu :shifted | self changesMenu: menu shifted: shifted from: src to: dest ].
	src classesMenu: [:menu :shifted | self classMenu: menu shifted: shifted from: src to: dest ].
	src methodsMenu: [:menu :shifted | self messageMenu: menu shifted: shifted from: src to: dest ].