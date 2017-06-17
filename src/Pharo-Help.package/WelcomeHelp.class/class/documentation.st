pages
documentation
	^ HelpTopic 
		title: 'More Documentation'
		contents: (self heading: 'More Documentation'), 
		
	'The most important documentation is the one you can get by exploring the system, reading class comments and looking for examples. 
You can find "in image" help by clicking on the following expression:

', (self doItFrom: 'HelpBrowser open.'), '

This browser is also accessible from the World>Help>Help Browser menu (just click the desktop, select Help, and then Help Browser).

A compendium of Pharo documentation can be found at: 

', (self url: 'http://pharo.org/documentation'), '

There you can find:
- Pharo books: ', (self url: 'http://books.pharo.org'), '
- Screencasts: ', (self url: 'https://www.youtube.com/channel/UCp3mNigANqkesFzdm058bvw'), '
- Presentations: ', (self url: 'http://www.slideshare.net/pharoproject/')