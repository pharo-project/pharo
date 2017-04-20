pages
exploreEnvironment
	^ HelpTopic 
		title: 'Explore Pharo Environment'
		contents: (self heading: 'Explore Pharo Environment'), 
		
		'The best way to learn Pharo is to explore it by your self. This is live objects all the way down: they can be inspected, browsed, changed... everything in the environment you have in your hands.
But sometimes it can be hard to getting you started with the tools and capabilities the environment gives you.

The most complete and updated guide for the Pharo environment can be found at "Updated Pharo by Example" free book. It is still a work in progress, but most of it is already done. You can find the book here: 

', (self url: 'https://github.com/SquareBracketAssociates/UpdatedPharoByExample'), '

One particular chapter that is interesting is the one that gives you a quick tour for all the environment: 

', (self url: 'https://ci.inria.fr/pharo-contribution/job/UpdatedPharoByExample/lastSuccessfulBuild/artifact/book-result/PharoTour/PharoTour.pdf')