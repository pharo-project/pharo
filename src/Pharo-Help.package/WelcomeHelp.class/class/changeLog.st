pages
changeLog
	^ HelpTopic 
		title: 'ChangeLog'
		contents: (self heading: 'New Stuff in Pharo 6.0'),
'- The PharoVM and image are now provided in 64-bit version in Linux and OSX and bring even better performance and stability
- A new code changes management system named Epica for easier reviewing and recovering of your code
- Integrated support for Git and easy-to-use tool for repositories and commits management named Iceberg (as preview for Pharo 6, it will be the default for Pharo 7)
- Unified foreign function interface (UFFI) for interfacing with the outside world was significantly improved
- The PharoVM is now part of OpenSmalltalk initiative
- Introduction of objects immutability, alternative bytecode sets and block closures independent on outer context
- The whole Pharo is now able to be bootstrapped from source codes managed by Git and Pharo modularity was improved
- The Dark Theme was improved and set as default color theme for Pharo

', (self heading: 'All Issues'), 
'In Pharo 6 over 1400 fixes and enhancements was integrated.

The complete list of fixed issues is too big to be placed here, but you can review all issues at FogBugz issue tracker (', (self url: 'https://pharo.fogbugz.com'), ') (requires account).', 
(self subheading: 'Tools'),
'- Epicea - Code changes manager
- Iceberg - Git repositories manager
- GTInspector, GTDebugger and other tools are now based on FastTable to display list of items for better performances
- GToolkit, GTools update
- Quality Assistant improvements
- More reliable interruption by Cmd+.
- Playground variables are now visible from debugger
- Debugger temp names mapping is fixed
- Close all debuggers in taskbar context menu
- Run To Here in GTDebugger
- Filtering of the results and critiques in the MessageBrowser
- Improvements of the Dependency Analyzer
- Nautilus
        - Split large variable entries in the Variables menu
        - Deprecated methods are shown with strikethrough emphasis
        - Abstract classes are shown in italic with a slight color adjustment', 
(self subheading: 'VM related'), 
'- 64-bits support
- Improve host platforms management (32-bit/64-bit)
- Improved UnifiedFFI
- The PharoVM is now part of OpenSmalltalk initiative
- Introduction of objects immutability
- Introduction of FullBlockClosure which will help for future evolutions of Pharo
- Ephemerons support, introduce Ephemeron Registry
- Support of alternative bytecode sets and introduction of Sista Encoder, the encoder for the SistaV1 bytecode set. This will be the base of future Pharo''s improvements',
(self subheading: 'Reflectivity'), 
'- General improvements
- haltOnce is active by default per method. It does not require global turning on and it is managed from source code area in Nautilus
- Execution counter for message nodes in source code area in Nautilus
- API for Metalinks on AST nodes
- Mirror primitives (Those are reflection primitives which access object state without messaging them, see MirrorPrimitives class)
- Inlined method const can be implemented by Metalinks', 
(self subheading: 'Other'), 
'- Dark Theme improvements
- Improvement of theme change while windows are open
- Support of two double quotes inside comments
- Standalone Morphic worlds in separate windows
- Fix of several memory leaks
- Improve working directory structure (introduction of a pharo-local directory to includes Pharo directories as package-cache)
- Better autocategorisation of methods
- Introduction of a FuzzyMatcher for approximate string matching
- Glamour integration in Spec
- Renaming (Cmd+R) in Nautilus supports more AST nodes
- anObject asMethodConst to cache expressions dynamically
- GlobalIdentifier for computer identification
- NeoUUIDGenerator replace the old UUIDGenerator
- STON was improved and is now used by Monticello FileTree
- Storing of suspended announcements
- Improved newAnonymousSubclass
- Inheritable process specific variables
- Fuel improvements
- Enable <example> methods to be easily executed
- Support for <sampleInstance>
- New class and methods API for tags as replacement for categories and protocols
- TabMorph improvements
- Unify Dictionary APIs
- Package manifests improvements
- Improve RadioButton groups', 
(self subheading: 'Cleanups'), 
'- Object>>#name is now deprecated and will be removed in Pharo 7
- Better system modularization
- Ability of the system to be fully bootstrapped from source codes
- Turn of catalog search in Spotter by default (This improve the stability of Pharo under poor internet connection)
- Remove Chroma-CubeHelix and TxWorkspace
- Rename Pragma>>#selector to Pragma>>#methodSelector
- Improve icons management (#iconNmaed: was introduce to replace DNUs)
- Limit use of #asClass in order to rely on an environment
- It is now possible to give a rewrite rule when deprecating a method to automatically rewrite code with deprecation (#deprecated:transformWith:)
- Deprecation of:
        Object>>name
        ShortRunArray class
        Object>>confirm:orCancel:
        Object>>ifNil:ifNotNilDo:
        Object>>ifNotNilDo:
        Object>>ifNotNilDo:ifNil:
        Collection>>ifEmpty:ifNotEmptyDo:
        Collection>>ifNotEmptyDo:
        Collection>>ifNotEmptyDo:ifEmpty:
        SequenciableCollection>>copyLast:
        Integer>>asBytesDescription
        Pragma>>method:', 
(self subheading: 'Unit testing/Documentation'), 
'- RecursionStopper: It provides an easy way to check if we are in a recursion and execute code just once in a recursion.
- New process specific variable CurrentExecutionEnvironment with values: DefaultExecutionEnvironment by default and TestExecutionEnvironment during test run
- SUnit improvements: time limit for tests, preventing "forked debuggers"
- New assert extension to compare floats by closeTo:
- More class comments and documentation', 
(self subheading: 'Network'), 
'- Support Server Name Indication (SNI) in Zodiac/SSLPlugin
- Zinc/Zodiac update

You can see the Pharo 6.0 changelog at: 

', (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo60ChangeLogs.md')