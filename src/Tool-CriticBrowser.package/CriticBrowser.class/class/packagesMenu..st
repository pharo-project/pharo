instance creation
packagesMenu: aBuilder
	<nautilusGlobalPackageMenu>
	(aBuilder item: #'Critic Browser')
		icon: self icon;
		action: [
			self  
				openOnRule: (ReRuleManager uniqueInstance allRules) 
				onEnvironment: 
					(RBPackageEnvironment new 
						packages: aBuilder model selectedPackages;
						yourself) ];
		help: 'Running critics rules on this package'