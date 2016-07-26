scenarios
scenariosForTests
	"
	1. select a configuration, then unload the configuration ... in OB you gtt an error because the 
		project was undefined .... simulate by creating a ConfigurationOf class that is not a real configuration
	2. add new configuration should associate the chosen repository with the configuration, too...maybe 
		addConfigurationTest should check this
	3. add new configuration then do checkpoing dev and note that dirty * is still on the label in the browser
	"