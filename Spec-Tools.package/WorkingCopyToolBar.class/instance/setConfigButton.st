initialization
setConfigButton

	configButton
		state: false;
		label: '+Config';
		help: 'Add a new metacello configuration';
		action: [ self createConfiguration ]