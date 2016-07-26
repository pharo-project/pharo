lessons
lesson11Blessing
	^Lesson
		title: 'Lesson 11 (Blessing)'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version11:.

"A version can be tagged with a blessing like #alpha, #beta, #release, #development or any 
other tag that you find useful. The blessing for version 1.1 is #development"

	(MetacelloTutorialConfig project version: ''1.1'') blessing.

"The default blessing is #release, so even though we didn''t specify a blessing for version 
0.5, the blessing is set:"

	(MetacelloTutorialConfig project version: ''0.5'') blessing.
	
"For version 1.1, it is important to explicitly set the blessing, because it imports version
''1.0-baseline'' whose blessing is #baseline:"

	(MetacelloTutorialConfig project version: ''1.0-baseline'') blessing.

"Blessings can be used as a filter. For example, you will notice that the result of the 
following expression is version 0.6, because #stableVersion answers the latest version whose 
blessing is _not_ #development, #broken, or #blessing: "

	MetacelloTutorialConfig project stableVersion.
	MetacelloTutorialConfig project stableVersion load.

"The blessing of version 1.1 is #development. To find the latest #development version you 
would execute this expression:"

	MetacelloTutorialConfig project latestVersion: #development.
	(MetacelloTutorialConfig project latestVersion: #development) load.

"You can get the very last version independent of blessing by executing this expression:"

	MetacelloTutorialConfig project bleedingEdge.
	MetacelloTutorialConfig project bleedingEdge load.
	
"In general, the #development blessing should be used for any version that is unstable. Once a 
version has stabilized, a different blessing should be applied.

The following expression will load the latest version of all of the packages for the latest 
#baseline version:"

	(MetacelloTutorialConfig project latestVersion: #baseline) load.

"Since the latest #baseline version should reflect the most up-to-date project structure, executing
the previous expression should load the absolute bleeding edge of the project. "

ProfStef next.
'