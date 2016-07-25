instance creation
packagesMenu: aBuilder
	<nautilusGlobalPackageMenu>
	(aBuilder item: #'Critic Browser')
		action: [
			self  
				openOnRule: (RBCompositeLintRule allGoodRules) 
				onEnvironment: 
					(RBPackageEnvironment new 
						packages: aBuilder model selectedPackages;
						yourself) ];
		help: 'Running critics rules on this package'