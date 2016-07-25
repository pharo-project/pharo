initialization
setCurrentConfigurationModel
	currentConfigurationModel  
		state: false;
		label: 'Uses Current Configuration';
		action: [ CriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current. 
				self delete ]