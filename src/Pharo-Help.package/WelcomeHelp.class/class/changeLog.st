pages
changeLog
	^ HelpTopic 
		title: 'ChangeLog'
		contents: (self heading: 'Highlights (aka New Stuff) in Pharo 7.0alpha'),
'- moving the development process to git and github (see ',(self url: 'https://github.com/pharo-project/pharo'),')
- bootstraping of a smaller image that will not include compiler/parser (Compiler and related packages are exported and loaded using a binary exporter named Hermes)
- integration of Renraku quality framework
- integration of WebBrowser
- integration of ReferenceFinder

', (self heading: 'All Issues'), 
'Over ... fixes and enhancements were integrated in this release.

As the complete list of fixed issues is too large to be placed here, you can review it on the FogBugz issue tracker (', (self url: 'https://pharo.fogbugz.com'), ') (requires account).', 
(self subheading: 'Tools'),
'- Improved Iceberg Git repositories manager
', 
(self subheading: 'VM related'), 
'- ...',
(self subheading: 'Reflectivity'), 
'- General improvements
- ...
', 
(self subheading: 'Other'), 
'- ...', 
(self subheading: 'Cleanups'), 
'- Object>>#setPinned: is now private
', 
(self subheading: 'Unit testing/Documentation'), 
'- More class comments and documentation', 
(self subheading: 'Network'), 
'- Zinc/Zodiac update

You can see the Pharo 7.0 changelog at: 

', (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo70ChangeLogs.md')
