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
		title: 'Changes in Pharo 9'
		contents:
		(self heading: 'Changes in Pharo 9') , 
'Pharo 9 is a massive release of Pharo. It brings a large set of features and increases the stability, speed, and versatility of Pharo.  

Here are the key features of Pharo 9:   

- Full redesign of the Spec UI framework:
	- new logic, application, style, GTK back-end
- NewTools: new playground, new inspector, new debugger
- New error infrastructure and emergency debugger)
- New composable completion framework
- General speed up 
- Compiler optimisations
- Better Refactorings
- Better parser for error recognition
- Fluid (composable) class syntax
- Comments in Microdown format (Markdown compatible)
- Fast universal FFI (Foreign Function Interface)
- Idle VM + SDL20 and back-end 
	- Extended event handling, including trackpad support
- ARM 64 bits 
- Full block closures
- Much more more tests

Now in details. 

', 
(self heading: 'Amazing development activity'),
' Pharo 9 got an amazing development activity. It saw the raise of enhancements and bug fixes.  
- 252 forks on github
- 1437+ issues fixed.
- 2231+ Pull requests integrated, not counting separately managed projects such as:
	- ',(self url: 'http://github.com/pharo-spec/Spec'),'
	- ',(self url: 'http://github.com/pharo-spec/NewTools' ),'
	- ',(self url: 'http://github.com/pharo-spec/Iceberg'),'
	- ',(self url: 'http://github.com/pharo-spec/opensmalltalk'),
(self subheading: 'General Speed up'),
'Pharo 9 got a general speed up due to Pragma speed, compiler improvements and the simplification of the UI framework.', 
(self subheading: 'Pharo 9 generalizes executable comments'),
'You read the method and you systematically get examples to show you how to use the message.  
And you can execute the comment (they are also accesible as tests through the DrTest GUI)! ', 
(self subheading: 'New and enhanced refactorings'),
'Pharo 9 did a large effort on refactorings.  
Pharo 9 is shipped with new refactorings (such as Extract to Setup XXX more?).  
Many refactorings got enhanced. Extract method can now extract a method out of methods in a full hierarchy in one click.  
Refactorings are now accessible inplace i.e., instance variables, class variables...', 
(self subsubheading: 'New refactorings'),
'1. Extract setUp method 
2. Remove senders of method refactoring
3. Copy package as refactoring 
4. Rename package (rename manifest)
5. Merge instance var x in y
6. Move to class side method
7. Create accessors with lazy initialization', 
(self subsubheading: 'Improved refactors'),
'1. Deprecate method (simple version)
2. Deprecate class
3. Extract method refactoring (comparing btw symbols and strings, show occurrences replaced)
4. Replace senders by another
5. Rename vars (traits)
6. Convert temporary to instance variable (check in the methods and methods of the subclasses if the same name of the temporary variable is used)
7. Push up method refactoring (improve messages)
8. Add access to pushUp and pushDown refactorings from source code
9. Permute parameters when add an argument
10. Abstract instance variable (now applies abstract methods when there is a reference to more than one variable in a method)

Migrate commands to commander2
- Change to use specific scopes (call refactorings with a selected scope as model)
	- Add previews to refactorings
- Expose variable refactoring commands
- Auto select protocol selected when rename

', (self heading: 'Amazing documentation activity'),
'A large set of new books is arriving with Pharo 9. Documenting many aspects of programming
in Pharo:
- Testing in Pharo
- Version control in Pharo
- Calling Foreign Functions with Pharo 
- Pharo with Style
- Zinc 
- Concurrent Programming in Pharo
- Commander 2.0
- Pharo by Example 9 soon out. 

', 
(self heading: 'New Interface Builder'),
'After 7 years of Spec was redesigned and drastically improved.  
Spec 2.0 is the foundation for all the tools of Pharo.  
It is faster and comes with:
- Different layouts and drop the slow interpreter design
- Application concept introduced (support groups of windows)
- New dialog builder
- Styling support
- Dynamic layouts are now simple and the norm
- Transmissions 
- Support for roassal and cairo drawings
- Tests
- Multiple back-ends: GTK and Morphic

', (self heading: 'New tools'),
'Pharo 9 comes with three redesigned tools: the inspector, the playground, and a new debugger.  
- Pharo arrives with a brand new inspector that can be extended based on Spec 2.0. Following the GTInspector, any object can have dedicated panes. Now defining advanced panes is as simple as defining a new spec presenter.
- The New playground supports variable sharing between multiple playgrounds.
- The New debugger supports
	- Unique object-centric debugging features: you can halt on only one specific object
	- A plugin architecture for scripting
	- A rewamped emergency debugger
	- Architecture to plug different debuggers
-  Pharo 9 includes a new code completion framework opening doors to statically learning. 
In addition, the Pharo parser has been enhanced to support better error handling. 
- Pharo 9 is has support for extra larges images (ones that can contain 30 millions lines of code).
Special search and completions are provided. 
- An extensible commander framework has been designed and is now in use.

', (self heading: 'Large clean up effort'),
' Pharo 9 got a large clean up effort. 
- Glamour and GTTools as well as their private widget sets got deprecated and  replaced by new tools. 
- Spec1 is still deprecated all tools are being rewrite to take advantage of Spec 2.0 new features.

', (self heading: 'Language improvement'),
'Pharo 9 comes also with a set of new language features. ', 
(self subheading: 'Slots'),
'First a continuous effort has be made to better support for first class instance variables (Slots)', 
(self subheading: 'Full block closures'),
'All the compiler chain and debugger got adapted to support the new format of block closures.
Such new format will enable to put in production dynamic optimisers such as Scorch or variations. It means that future Pharo versions will be able to have modern JIT compiler architecture with a speculative optimizers and not just a baseline JIT.', 
(self subheading: 'New class definition'),
'A new class definition has been designed as presented at ESUG in Slovenia.
It is based on a fluid API that controls combinatorial explosion of features.

',
(self sourceCode:'SpAbstractPresenter << #SpPresenter
	trait: SpTPresenterBuilder;
	slots: {
			 #application .
			 #focusOrder .
			 #contextKeyBindings => SpObservableSlot .
			 #windowIcon => SpObservableSlot .
			 #aboutText => SpObservableSlot .
			 #extent => SpObservableSlot .
			 #styles };
	tag: ''Base'';
	package: ''Spec2-Core''')
,'

This class definition is optional in Pharo 9 and will be default version of Pharo 10.', 
(self subheading: 'Microdown'),
'Class comments are now written in Microdown a retro-compatible version of Markdown
extended with Pillar features. In addition any class can customise the rendering of the comments
and enhance it.  
Future help will be take advantage of Microdown and its architecture.', 
(self subheading: '"Dynamic call rewriting of deprecated methods" is now systematically used'),
'With Dynamic call rewriting of deprecated methods Pharo offers a unique feature that supports the automatic migration of your code at runtime. You run your tests or your application and you can get it automatically migrated to the new API.

', 
(self heading: 'More robust and enhanced virtual machine'),
'Pharo 9 saw a massive investment and results on the Virtual machine side.  
It is available for Windows (Intel 64bits/ARM 64bits), Linux (Intel 64bits / ARM 32 bits / ARM 64bits), OSX (Intel 64bits / ARM 64 bits).  
A new VM with
- Idle support.
	- No more pulling of events).
	- Better support for server and preserves battery.

- Primitive speed improvements in Windows.

- Full image side events handling is now based on SDL2. 
	- The complete and total event information is managed at the image level. 
	- It supports gesture definition.

- First fully working ARM-based JIT for Windows, Raspberian and Apple Silicon. 

- Image can be now embeddable in the executable optionally.

- More stability due to active chasing of difficult to track bugs. 
	- Bugs are now all covered by regression tests, even the assembly level, and code compaction. 

- Pharo have more than 3000 tests on the VM. These tests have been developed and are run on all platforms on any commits. 

- Builds using OBS: Pharo is built on Arch, Debian 10, Debian 9, Fedora 31, 32, 33, Raspbian 10, Raspbian 90, openSuse Leap 15.1, openSuse Leap 15.2, openSuse Tumbleweed, Ubuntu 18.04, 19.04, 20.04, 21.04

- Native windows compilation
	- New UFFI based on [libFFI](https://sourceware.org/libffi/) with speed improvements
	- Clean up of code, repetible generation of VM code
	- Unification of Socket implementations on the different platform
	- Optional compilation
	- Documentation
	- Cleanup of Plugins

', 
(self heading: 'New threaded architecture'),
'Pharo 9 comes also with a new Threaded FFI support. In fact Threaded FFI is not a single architecture but a set of alternatives that can be choosen depending on the constraints of the OS. TFFI offers adaptability to the different constraints imposed by Cocoa or GTK.	
		
You can see the Pharo 10.0 changelog at: 

', (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo100ChangeLogs.md')
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
