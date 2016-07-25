initialization
setResetModel
	resetModel 
		state: false;
		label: 'Reset Current Configurartion';
		action: [ SelectPackageBrowser open. 
				self delete ]