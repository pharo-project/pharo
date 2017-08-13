pages
gettingHelp
	^ HelpTopic 
		title: 'Getting Help'
		contents: (self heading: 'Getting Help'),
		'Pharo has a vibrant community that shares knowledge in different ways: 

- The "Pharo Users" mailing list: 
	', (self url: 'http://lists.pharo.org/mailman/listinfo/pharo-users_lists.pharo.org'), '
- The "Pharo Discord channel": 
	', 	(self url: 'http://discord.gg/Sj2rhxn'), '
- The "Pharo IRC Channel": 
	', (self url: 'irc.freenode.net, #pharo channel '), '

You can find more information, lists to browse/suscribe and places to share code at: 

', (self url: 'http://pharo.org/community')