"
I'm the help page for welcoming new users. 
I pass through basic concepts to getting started with Pharo.
"
Class {
	#name : #WelcomeHelp,
	#superclass : #CustomHelp,
	#category : #'Pharo-WelcomeHelp'
}

{ #category : #accessing }
WelcomeHelp class >> bookName [
	"the character zero is a hack to display it always first"
	^ (Character value: 0) asString, 'Welcome'
]

{ #category : #pages }
WelcomeHelp class >> changeLog [
	^ HelpTopic 
		title: 'Changes in Pharo 10'
		contents: (
			(self section: 'Changes in Pharo 10 ' with: [ self sectionIntro ]), 
			(self section: 'Activity' with: [ self sectionActivity ]), 
			(self section: 'General' with: [ self sectionGeneral ]), 
			(self section: 'Spec' with: [ self sectionSpec ]), 
			(self section: 'NewTools' with: [ self sectionNewTools ]), 
			(self section: 'Roassal' with: [ self sectionRoassal ]), 
			(self section: 'Virtual Machine' with: [ self sectionVM ]),
			(self section: 'Details' with: [ self sectionDetails ]))
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

{ #category : #pages }
WelcomeHelp class >> gettingHelp [
	^ HelpTopic 
		title: 'Getting Help'
		contents: (self heading: 'Getting Help'),
		'Pharo has a vibrant community that shares knowledge in different ways: 

- The "Pharo Users" mailing list: 
	', (self url: 'https://lists.pharo.org/empathy/list/pharo-users.lists.pharo.org'), '
- The "Pharo Discord channel": 
	', 	(self url: 'https://discord.gg/QewZMZa'), '

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
		parent: #PharoHelp;
		order: 1;
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
	window extent: 700@500.
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
	^ #(welcome changeLog learn exploreEnvironment documentation gettingHelp)
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
		aStream 
			nextPutAll: 'commit: ';
		   nextPutAll: version commitHashShort;
			nextPutAll: ', date: ';
			nextPutAll: version date asString]
]

{ #category : #'private - sections' }
WelcomeHelp class >> section: heading with: aBlock [ 

	^ 
(self heading: heading), 
aBlock value, '
	
'
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionActivity [

	^ 'Current Pharo development status.  
- 275 forks on github
- 600+ issues fixed.
- 700+ Pull requests integrated, not counting separately managed projects such as:
	- ' asText, (self url: 'http://github.com/pharo-spec/Spec'),'
	- ',(self url: 'http://github.com/pharo-spec/NewTools' ),'
	- ',(self url: 'http://github.com/pharo-spec/Iceberg'),'
	- ',(self url: 'http://github.com/pharo-spec/opensmalltalk-vm')
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionDetails [

	^ 'You can see the Pharo 10.0 changelog at: 

' asText, (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo100ChangeLogs.md')
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionGeneral [
			
	^ '- Fluid class definition greatly improved (and now functional, even if not activated by default)
- Improve refactors (push down, extract method, ...)
- Improve code critiques (and migrated to Spec2)
- Improve FileReference (fixes in relative Paths, canonicalization of names)
- Improve quality rules (test categorisation, better explanation of rules) 
- Improve debugging infrastructure (sindarin, 
- Fixes in compiler (when using whileTrue:)
- Fixes on shortcut printing (on OSX)
- Fixes undo/redo in the rubric editor
- Microdown V2 (and beautiful comments) revamped
- Enhanced FFI: exposing the function ABI, add TFProcessLocalWorker.
- All repeated literals in image are now shared.
- Enhanced autocompletion support for large images.
- Fuel serializer now supports stateful traits.
- Metacello implements the function "lock" to avoid further modifications to a loaded project.
- Modularisation work: Keymapping, Freetype, ThreadedFFI, Athens, TaskIT
- Networking: improve ipv6
- Removed GTTools
- Removed Glamour
- Removed Spec1
- Removed unused FFI package (Alien, most of FFI-Kernel)
- Removed old EncoderForV3PlusClosures
- ... and many (many for real) clean ups and small enhancements that would be impossible to sumarize here.'
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionIntro [
			
	^ 'A large international community of developers worked hard to prepare a new release of the Pharo platform.

Here are the key features of Pharo 10:   

- Spec 2 major release	
	- all layouts are now dynamic
	- API cleaning	
	- add support for more presenters
- A lot of cleaning
	- lots of code removed (image and VM)
	- Image is smaller (58 instead ion 66 MB)
- Removal of Spec 1 (tools written in Spec 1 have been rewritten in Spec 2)
- Removal of Glamour and GTTools (were not maintained anymore and got rewritten in Spec 2)
- Drop support of V3PlusClosures byte code set
- Removal of unused FFI packages
- Improved debugging infrastructure
- Zinc (HTTP client/server) components update
- Roassal 3 update

Now in details.'
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionNewTools [
			
	^ '- For the new tools already included in the image, this was largely a maintenance release, with a lot of small improvements in performance and usability.
- Spotter has received several iterations looking to improve its reliability and responsibility. It now incorporates also some improvements oriented to large images too.
- Playground has incorporated hooks allowing better extensibility and received several fixes on how code works (mostly due to fixes on `SpCodePresenter`).
- Debugger received some stability fixes. 
- We incorporated also other migrated tools: Dependency Analyser, Critiques Browser, Font Chooser and several minor tools, allowing the removal of Spec1.
- Increment the amount of tests.
- Fixed printing of object information in inspector, to prevent huge chunks of text.'
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionRoassal [
			
	^ '- The visualisation engine now is in version v0.9.9f, full changelog can be seen here: ' asText, (self url: 'https://github.com/ObjectProfile/Roassal3/releases')
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionSpec [
			
	^ '- Deprecation of class side defaultSpec, now all layouts can be defined in instance side (but defaultLayout can still be defined in class side, in case of need)
- Deprecation of all withSpec suffixes. Now to open a presenter window you just use #open/#openModal, etc. (this was a lot more work as you may think).
- Added #asWindow, #asModalWindow to allow user to modify the window presenter (adding extent, ot title, for example) without needing to override `initializeWindow:` and before the window is opened.
- All layouts are dynamic and can be modified at run time (API depending the layout). 
- Enhancement SpApplication with notion of iconProvider
- Added some standard dialogs (inform, request, confirm) to SpApplication
- Added a progress bar standard dialog/processor (SpJobListPresenter)
- Enhanced CommonWidgets: SpFilteringListPresenter, etc.
- Many improvements to SpCodePresenter, including syntax highlight, better code operations, text search, etc. 
- Implemented drag&drop (not all widget, but required).
- Enhanced styleSheets usage
- Box, grid and scrollable layouts implement SpTAlignable (to better place its contents).
- Enhanced API for building grid layouts.
- Fix all remaining memory leaks (well, some may remain, as always... but we removed all we found ;)'
]

{ #category : #'private - sections' }
WelcomeHelp class >> sectionVM [
			
	^ 'Virtual Machine is now v9.0.13, full changelog can be seen here: ' asText, (self url: 'https://github.com/pharo-project/opensmalltalk-vm/blob/pharo-9/CHANGELOG.md'), '

- Improving the Socket implementation:
    - Better Out of Band Data Handling
    - Better Name Resolution
    - Faster Handling of Network Events in OSX
- Improvements in the Asynchronous I/O Support and native implementations for each OS
- Improvements in FFI
    - Multiple ABI support
    - Better Marshalling of LargeIntegers
- Improving Debugging in Windows
- Automatic Build and Test in all Platforms (OSX Intel & ARM / Linux Intel & ARM 32/64 / Windows Intel)
- Message Profiling
- Fixes in Floats ARM 32 Bits
- Cleanups / Tests 
- Third Party Libraries Update'
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
(self doItFrom: 'PharoDarkTheme beCurrent' text: 'Dark Theme'), ' and ',
(self doItFrom: 'Display toggleFullscreen ' text: ' switch fullscreen mode')
,' 

Click if you need to ',
(self doItFrom: 'WelcomeHelp openNetworkSettings' text: 'setup a network proxy'), '.

You can also ',
(self doItFrom: 'SettingBrowser open' text: 'apply many other other settings').
]
