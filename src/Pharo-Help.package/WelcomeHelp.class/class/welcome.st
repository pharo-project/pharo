pages
welcome
	^ HelpTopic 
		title: 'Welcome to Pharo 6.0'
		contents: (self heading: 'Pharo 6.0'), 
		
'Welcome to Pharo, an immersive live programming environment.

Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

For more information, please visit here: 

', (self url: 'http://pharo.org'),
(self subheading: 'Color themes'),
'White theme: ', (self doItFrom: 'Pharo3Theme beCurrent. '), ' 
Dark theme: ', (self doItFrom: 'Pharo3DarkTheme beCurrent. ').