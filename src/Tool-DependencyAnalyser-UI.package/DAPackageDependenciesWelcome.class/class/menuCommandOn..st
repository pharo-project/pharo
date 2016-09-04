world menu
menuCommandOn: aBuilder
	<worldMenu>

	(aBuilder item: #PackageDependencies)
		order: 0.21;
		parent: #Tools;
		label: 'Dependency Analyser';
		icon: (self iconNamed: #packageIcon);
		action: [ self new open ]