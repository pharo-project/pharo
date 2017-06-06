pages
changeLog
	^ HelpTopic 
		title: 'ChangeLog'
		contents: (self heading: 'Highlights (aka New Stuff) in Pharo 6.0'),
'- The PharoVM and image are also provided in a 64-bit version in Linux and macOS/OSX and bring even better performance and stability
- A new code changes management system named Epicea for reviewing and recovering of your code easily
- Integrated support for Git through an easy-to-use tool for repositories and commits management named Iceberg (as a preview in Pharo 6, it will be the default in Pharo 7)
- The unified foreign function interface (UnifiedFFI) for interfacing with the outside world is significantly improved
- The PharoVM is now part of OpenSmalltalk initiative
- Introduction of object immutability, alternative bytecode sets and block closures independent of outer context
- Pharo can now be bootstrapped from source code managed by Git
- Pharo modularity is improved
- Pharo is faster
- The Dark Theme was improved and set as default color theme of Pharo

', (self heading: 'All Issues'), 
'Over 1400 fixes and enhancements were integrated in this release.

As the complete list of fixed issues is too large to be placed here, you can review it on the FogBugz issue tracker (', (self url: 'https://pharo.fogbugz.com'), ') (requires account).', 
(self subheading: 'Tools'),
'- Epicea provides a code changes manager
- Iceberg provides a Git repositories manager
- GTInspector, GTDebugger and other tools are now based on FastTable (long lists of items are rendered much faster)
- GToolkit and GTools have been updated
- Quality Assistant has been improved
- Interrupt key (Cmd+ /, Ctrl+.) has been made more reliable
- Playground variables are now visible from debugger
- Debugger temp names mapping is fixed
- There is a "Close all debuggers"  in the taskbar context menu
- GTDebugger has a "Run to here" feature
- Results and critiques can be filtered in the MessageBrowser
- Dependency Analyzer has been improved
- Nautilus enhancements
        - Splitting of large variable entries in the Variables menu
        - Deprecated methods are shown with strikethrough emphasis
        - Abstract classes are shown in italics with a slight color adjustment', 
(self subheading: 'VM related'), 
'- 64-bits support
- Improvement of host platforms management (32-bit/64-bit)
- Improved UnifiedFFI
- The PharoVM is now part of OpenSmalltalk initiative
- Introduction of object immutability
- Introduction of FullBlockClosure which will help in future evolutions of Pharo
- Ephemerons support, introduction of the EphemeronRegistry
- Support of alternative bytecode sets and introduction of Sista Encoder, the encoder for the SistaV1 bytecode set. This will be the bedrock on which Pharo will improve',
(self subheading: 'Reflectivity'), 
'- General improvements
- haltOnce is active by default per method. It does not require global turning on (enable haltOnce) and it is managed from the source code area in Nautilus
- Execution counter for message nodes in the source code area in Nautilus
- API for Metalinks on AST nodes
- Mirror primitives (Those are reflection primitives which access object state without messaging them, see MirrorPrimitives class)
- Inlined method const can be implemented by Metalinks', 
(self subheading: 'Other'), 
'- Dark Theme improvements
- Improvement of theme change while windows are open
- Support of two double quotes inside comments
- Standalone Morphic worlds in separate windows
- Fix of several memory leaks
- Improvement of working directory structure (introduction of a ''pharo-local'' directory to include Pharo directories such as ''package-cache'')
- Better autocategorization of methods
- Introduction of a FuzzyMatcher for approximate string matching
- Glamour integration in Spec
- Renaming (Cmd+R / Ctrl+R) in Nautilus supports more AST nodes
- anObject asMethodConst to cache expressions dynamically
- GlobalIdentifier for computer identification
- NeoUUIDGenerator replaces the old UUIDGenerator
- STON was improved and is now used by Monticello FileTree
- Storage of suspended announcements
- Improved newAnonymousSubclass
- Inheritable process specific variables
- Fuel improvements
- Enablement of <example> methods so that they can be executed easily
- Support for <sampleInstance>
- New class and method API for tags as replacement for categories and protocols
- TabMorph improvements
- Unification of Dictionary APIs (including an OrderedDictionary)
- Package manifests improvements
- Improvement of RadioButton groups', 
(self subheading: 'Cleanups'), 
'- Object>>#name is now deprecated and will be removed in Pharo 7
- Better system modularization
- Ability for the system to be fully bootstrapped from source code
- Turn off of catalog search in Spotter by default (This improves the stability of Pharo when used with poor Internet connections)
- Removal of Chroma-CubeHelix and TxWorkspace
- Rename of Pragma>>#selector to Pragma>>#methodSelector
- Improvement of icons management (#iconNamed: introduced in order to replace DNU-based icons)
- Limit use of #asClass in order to rely on an environment
- It is now possible to give a rewrite rule when deprecating a method to automatically rewrite code with deprecation (#deprecated:transformWith:)
- Deprecation of the following:
        Object>>name
        ShortRunArray class
        Object>>confirm:orCancel:
        Object>>ifNil:ifNotNilDo:
        Object>>ifNotNilDo:
        Object>>ifNotNilDo:ifNil:
        Collection>>ifEmpty:ifNotEmptyDo:
        Collection>>ifNotEmptyDo:
        Collection>>ifNotEmptyDo:ifEmpty:
        SequenceableCollection>>copyLast:
        Integer>>asBytesDescription
        Pragma>>method:', 
(self subheading: 'Unit testing/Documentation'), 
'- RecursionStopper provides an easy way to check if we are in a recursion and execute code just once in a recursion
- New process specific variable ''CurrentExecutionEnvironment'' with value DefaultExecutionEnvironment by default and TestExecutionEnvironment during a test run
- SUnit is improved by introducing a time limit for tests, preventing "forked debuggers"
- New assert extension to compare floats with #closeTo:
- More class comments and documentation', 
(self subheading: 'Network'), 
'- Support Server Name Indication (SNI) in Zodiac/SSLPlugin
- Zinc/Zodiac update

You can see the Pharo 6.0 changelog at: 

', (self url: 'https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo60ChangeLogs.md')