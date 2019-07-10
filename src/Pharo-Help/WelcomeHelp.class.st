"
I'm the help page for welcoming new users. 
I pass through basic concepts to getting started with Pharo.
"
Class {
	#name : #WelcomeHelp,
	#superclass : #CustomHelp,
	#category : #'Pharo-Help'
}

{ #category : #accessing }
WelcomeHelp class >> bookName [
	"the character zero is a hack to display it always first"
	^ (Character value: 0) asString, 'Welcome'
]

{ #category : #pages }
WelcomeHelp class >> changeLog [
	^ HelpTopic 
		title: 'Changelog'
		contents: (self heading: 'Changelog highlights'),
'- Pharo development moved to Github and Pharo is now bootstrapped
- 64-bit VM for Windows as a technical preview
- Introduction of a modular implementation of Stateful traits (see user documentation ',(self url: 'https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Traits.md'),')
- Replacement of Nautilus by Calypso System Browser (',(self url: 'https://github.com/pharo-ide/Calypso'),') 
- A new version of Iceberg (',(self url: 'https://github.com/pharo-vcs/Iceberg'),') with a new interface more user-friendly
- Migration of Streams to the Zinc streams implementation that is more modular'
, 
(self subheading: 'Revolutionary kernel changes'), 
'- Pharo is now bootstrapped
- Modular stateful traits
- Traits flattening in kernel packages
- New sources file for every bootstrapped version, empty changes
- New class builder
- Binary packages loading (Hermes)
- Use of Zinc streams as default streams 
  - migration guideline: ',(self url: 'https://github.com/pharo-open-documentation/pharo-wiki/blob/master/Migration/MigrationToPharo7.md')
, 
 (self subheading: 'Infrastructure changes'), 
'- Switch to GitHub
- Tonel code format 
- Update building infrastructure to Jenkins 2'
, 
(self subheading: 'Code management'), 
'- Introduction of Calypso, the new system browser
  - New navigation model    
  - Faster UI
  - Tabs toolbar instead of single source code panel
  - Explicit commands (e.g., add new method) instead of duplicated menu and shortcuts
  - Extendable by plugins
  - Suitable for the remote scenario
  - "Dynamic protocols", supporting multiple tags per method
  - No longer requires star convention for class extension
  - Visibility option for inherited methods
  - Variable view as the special mode for method group view.
  - Better keyboard navigation
  - And many more improvements'
, 
(self subheading: 'Iceberg'), 
'- A new friendly user interface
- Simplification of solving problems with repositories
- Workflow simplification
- Better GitHub, BitBucket, GitLab support
- Improved code merging and comparisons
- New code serialization format (Tonel)
- Repositories metadata
- Documentation
- Better error handling, tags support
- Improved Iceberg-Metacello integration
- Credentials store'
, 
(self subheading: 'Cleanups'), 
'- Remove Nautilus
- Remove UpdateStream
- Remove all Configurations
- Remove ShoreLine-Report
- Remove TxText
- Remove PluggableTabBarMorph
- Remove the old Compiler
- Remove Watery theme
- Remove Versionner
- Remove Komitter
- Remove LegacyWeakSubscription
- Continue on deprecation of FileStream
- Dead code cleanups
- Code organization cleanups
- Unify categorization of common methods
- Cleanups of object inspection protocol
- Start Ring deprecation
- Better structure of packages
- Add more package comments'
, 
(self subheading: 'Updates'), 
'- Zinc
- Epicea
- FFI
- Metacello
- Fuel'
, 
(self subheading: 'New projects'), 
'- Integration of Calypso
- Integration of WebBrowser, a utility to open the default web browser of the user on an URL
- Integration of ReferenceFinder to find reference paths between objects to help find memory leaks
- Integration of Hermes, a binary code loader for Pharo''s bootstrap
- Integration of Refactoring 2, a re-architecture of the Refactoring project
- Integration of Commander, a project to implement Command design pattern
- Integration of ClassAnnotation, a reusable mechanism for first-class annotations in Pharo'
, 
(self subheading: 'Spec'), 
'- Renamed ComposableModel into ComposablePresenter and changed the suffix of all subclasses
- New calendar presenter 
- Spec-Demo (Under Help menu entry)
- Improved Spec linking to domain models'
, 
(self subheading: 'Look & Feel'), 
'- Uses the white theme by default
- Improves themes switching
- Uses a Main menu bar to make Pharo more standard to other applications
- Improvement of the design of the Taskbar
- New window management shortcuts
- Better support of fallback bitmap fonts
- Improved Inspector refreshing
- Spotter is now looking for opened windows
- Support for display scale factor
- FastTable improvements'
, 
(self subheading: 'VM interface'), 
'- 64-bit VM for windows as a technical preview
- DoubleWord and DoubleByte arrays
- Added EncoderForSistaV1
- Added read-only literals support
- Improvements and fixes for 64-bit support
- Fixes for FullBlockClosure
- Support of the new bytecode set
- Uses FFI to bridge Pharo with FreeType'
, 
(self subheading: 'Reflectivity'), 
'- Breakpoint improvements
- Introduction of Breakpoint browser (under the Debugging menu bar entry)
- Test coverage using metalinks
- Reflectivity and metalink improvements'
, 
(self subheading: 'Other'), 
'- The logic of the Finder was rewritten
- Startup now running in a fresh process
- Better pin messages
- NewValueHolder and Model API unification
- Better separation of working and image directories
- SortFunctions are now composable
- Better OSWindow support
- Converted rules to the Renraku architecture
- Class and method comment improvements
- Extensible Playground (pageActionOrder:)
- Optional password management for command line handlers
- System dependencies tests
- Unified way to obtain package manifests		
		
You can see the Pharo 7.0 changelog at: 

', (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo70ChangeLogs.md')

]

{ #category : #pages }
WelcomeHelp class >> documentation [
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
]

{ #category : #pages }
WelcomeHelp class >> exploreEnvironment [
	^ HelpTopic 
		title: 'Explore the Pharo Environment'
		contents: (self heading: 'Explore the Pharo Environment'), 
		
		'The best way to learn Pharo is to explore it by yourself. This is live objects all the way down: they can be inspected, browsed, changed... everything in the environment you have in your hands.
But sometimes it can be hard to get started with the tools and capabilities the environment provides.

The most complete and updated guide for the Pharo environment can be found in the "Updated Pharo by Example" free book. It is still a work in progress, but most of it is already done. You can find the book here: 

', (self url: 'https://github.com/SquareBracketAssociates/UpdatedPharoByExample'), '

One chapter of particular interest is the one that provides a quick tour of the environment: 

', (self url: 'https://ci.inria.fr/pharo-contribution/job/UpdatedPharoByExample/lastSuccessfulBuild/artifact/book-result/PharoTour/PharoTour.pdf')
]

{ #category : #defaults }
WelcomeHelp class >> fastNetworkDefaults [
	"Allow to display catalog projects in spotter on regular and fast networks"
	CatalogSettings displayCatalogProjectsInSpotter: true.
	self inform: 'Catalog projects accessible through Spotter'
]

{ #category : #pages }
WelcomeHelp class >> gettingHelp [
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
]

{ #category : #pages }
WelcomeHelp class >> learn [
	^ HelpTopic 
		title: 'Learn Pharo'
		contents: (self heading: 'Learn Pharo'), 		
'You can learn Pharo by clicking on the following expression: 
	
', (self doItFrom: 'ProfStef go.'), 

(self subheading: 'MOOC'), 
'Pharo has an excellent MOOC (Massive Open Online Course). You can find more information here: 

', (self url: 'http://mooc.pharo.org'), 

(self subheading: 'Books'), 
'There are several free Pharo books that can be download here: 

', (self url: 'http://books.pharo.org'), '

A very interesting starting point would be looking into the "Updated Pharo by Example" free book. It is still a work in progress, but most of it is already done. You can find the book here: 

', (self url: 'https://github.com/SquareBracketAssociates/UpdatedPharoByExample'), '
'
]

{ #category : #'world menu' }
WelcomeHelp class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Welcome to Pharo')
		parent: #Help;
		order: 1.2;
		action: [ self open ];
		icon: ((self iconNamed: #pharoIcon) scaledToSize: 16@16);
		help: 'Welcome window for Pharo' 
]

{ #category : #showing }
WelcomeHelp class >> open [
	<script>
	^ (HelpBrowser openOn: self) next
]

{ #category : #showing }
WelcomeHelp class >> openForRelease [
	<script>
	| browser window findBlock |
	
	findBlock := [ :each | (each isKindOf: SystemWindow) and: [ each label = self bookName ] ]. 
	self currentWorld submorphs 
		detect: findBlock 
		ifFound: [ :oldWindow | oldWindow delete ].
	browser := self open.
	window := browser dependents detect: findBlock.
	window extent: 700@400.
	window center: Display extent / 2
]

{ #category : #'defaults - actions' }
WelcomeHelp class >> openNetworkSettings [

	| browser window path |	
	browser := SettingBrowser new 
		changePackageSet: { 
			#'Network-Kernel' asPackage.
	 };
		yourself.		 
	window := browser open.
	path := (window submorphs detect: [ :each | (each isKindOf: MorphTreeMorph) ]) nodeList.
	browser expandNodePath: path  
]

{ #category : #accessing }
WelcomeHelp class >> pages [
	^ #(welcome changeLog learn exploreEnvironment useExternalPackages documentation gettingHelp)
]

{ #category : #pages }
WelcomeHelp class >> pharoVersionString [

	| version | 
	
	version := SystemVersion current.

	^ String streamContents: [ :aStream  |
		version hasValidBuildNumber ifTrue: [ 
			aStream nextPutAll: 'build: '.
			version build printOn: aStream.
			aStream nextPutAll: ', '. ].
		aStream nextPutAll: 'commit: '.
		aStream nextPutAll: version commitHashShort ]
]

{ #category : #defaults }
WelcomeHelp class >> slowNetworkDefaults [
	"Do not display network bound catalog projects in spotter to keep it responsive."
	CatalogSettings displayCatalogProjectsInSpotter: false.
	self inform: 'Catalog projects not accessible through Spotter due to slow network. Use search in catalog tool to load external packages.'
]

{ #category : #pages }
WelcomeHelp class >> useExternalPackages [
	^ HelpTopic 
		title: 'Using External Packages'
		contents: (self heading: 'Using External Packages'), 'Pharo already comes pre-loaded with a rich set of packages that you can use to explore the system and develop your own applications. However there is also a huge library of user contributed projects that you can also load using the "Catalog Browser". To open it click on the following expression:
		
', (self doItFrom: 'CatalogBrowser open.'), '

This browser is also accessible from the World>Tools>Catalog Browser menu (just click the desktop, select Tools, and then Catalog Browser).

Catalog projects can be browsed online: 

', (self url: 'http://catalog.pharo.org'), '

You can also a rough list (not very friendly) of many packages available stored in smalltalkhub repository:

', (self url: 'http://smalltalkhub.com/list'), '

(this list is not a compendium of packages for Pharo but is a good place to start looking for something in particular and which happens to not be present in the catalog).

The catalog can be enabled in Spotter by enabling the "Display catalog projects in Spotter" system preference.
'
]

{ #category : #pages }
WelcomeHelp class >> welcome [
	^ HelpTopic 
		title: 'Welcome to ', Smalltalk version
		contents: (self heading: Smalltalk version withSubtitle: self pharoVersionString), 

'Welcome to Pharo, an immersive live programming environment.

Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

For more information, please visit here: ', (self url: 'http://pharo.org'),
(self subheading: 'Quick setup'),
'Choose your preferred color theme: ',
(self doItFrom: 'PharoLightTheme beCurrent' text: 'Light Theme'), ' or ',
(self doItFrom: 'PharoDarkTheme beCurrent' text: 'Dark Theme'), ' 

Click if you have access to a: ',
(self doItFrom: 'WelcomeHelp fastNetworkDefaults' text: 'regular network connection'), ' or ',
(self doItFrom: 'WelcomeHelp slowNetworkDefaults' text: 'slow network'), '
or if need to ',
(self doItFrom: 'WelcomeHelp openNetworkSettings' text: 'setup a network proxy'), '.

You can also ',
(self doItFrom: 'SettingBrowser open' text: 'apply many other other settings'), ' or extend the image ',
(self doItFrom: 'CatalogBrowser open' text: 'by loading additional external packages from catalog'), '.'
]
