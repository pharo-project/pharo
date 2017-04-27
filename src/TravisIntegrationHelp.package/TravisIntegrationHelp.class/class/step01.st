pages
step01
	^ HelpTopic 
		title: 'Step 01 - Configuration'
		contents: 
		(self heading: 'Step 01 - Configuration'),
		'You need a configuration for your project. A configuration contains the necessary to load your application, and Travis has to know about it.
		
Before moving to the next step, you need to have a working configuration. Note that it does not matter where your configuration is located (e.g., in your SmalltalkHub repository, or in MetaRepoForPharoXX).

In this help, we will use the ConfigurationOfRoassal2 located in ', (self url: 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo60/main')