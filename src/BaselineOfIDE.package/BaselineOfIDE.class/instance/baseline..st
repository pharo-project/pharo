baseline
baseline: spec 
    
	<baseline>

 	| repository | 
	
	repository := 'filetree://./pharo-core/src'.    
       
	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.
		
		spec baseline: 'BasicTools' with: [spec repository: repository].

		spec package: 'ReleaseTests'. 
		spec package: 'Refactoring-Critics'.
		spec package: 'NautilusRefactoring'.

		spec package: 'Ombu'.
		spec package: 'Epicea'.
		spec package: 'Hiedra'.
		spec package: 'EpiceaBrowsers'.
		spec package: 'HiedraTests'. 
		spec package: 'OmbuTests'.
		spec package: 'EpiceaTests'.
		spec package: 'EpiceaBrowsersTests'.

		
		spec package: 'Shout'.
		spec package: 'Reflectivity-Tools'.

		spec package: 'Rubric-Styling'.

		spec package: 'FuelTools-Debugger'.
		
		"spec package: 'Deprecated60'."
		spec package: 'Flashback-Decompiler'.
		spec package: 'BlueInk-Core'.
		
		spec package: 'Collections-Tests'.
		spec package: 'Text-Tests'.
		spec package: 'Network-Tests'.
		spec package: 'Tests'.
		spec package: 'Ring-Tests-Kernel'.
		spec package: 'AST-Tests-Core'.
		spec package: 'FileSystem-Tests-Core'.
		spec package: 'FuelTests'.
		spec package: 'Gofer-Tests'.
		spec package: 'Kernel-Tests'.
		spec package: 'Manifest-Core'.
		spec package: 'Manifest-Resources-Tests'.
		spec package: 'Monticello-Tests'.
		spec package: 'OpalCompiler-Tests'.
		spec package: 'Refactoring-Tests-Core'.
		spec package: 'Reflectivity-Tests'.
		spec package: 'ReleaseTests'.
		spec package: 'Ring-Tests-Containers'.
		spec package: 'Ring-Tests-Kernel'.
		
		spec package: 'System-Serial Port'.
		spec package: 'System-Localization'.
		
		spec package: 'Zinc-Resource-Meta-Tests'.

		spec package: 'Zodiac-Tests'.	
		spec package: 'Zodiac-Extra'.	
		spec package: 'Zinc-Zodiac'.	
			
		spec package: 'Metacello-GitBasedRepository'.	

		spec package: 'Metacello-Bitbucket'.		
		spec package: 'Metacello-TestsMCResources'.
		spec package: 'Metacello-TestsCore'.
		spec package: 'Metacello-TestsCommonMC'.
		spec package: 'Metacello-TestsMC'.	"standalone"
		spec package: 'Metacello-TestsMCCore'.	"standalone"
		spec package: 'Metacello-TestsPlatform'.	"standalone"
		spec package: 'Metacello-TestsReference'.	"standalone"

		spec package: 'RPackage-Tests'.

		spec package: 'Glamour-Announcements'.
		spec package: 'Glamour-Helpers'.
		spec package: 'Glamour-Core'.
		spec package: 'Glamour-Presentations'.
		spec package: 'Glamour-Browsers'.
		spec package: 'Glamour-Examples'.
		spec package: 'Glamour-Morphic-Brick'.
		spec package: 'Glamour-Tests-Core'.
		spec package: 'Glamour-Tests-Resources'.
		spec package: 'Glamour-Morphic-Theme'.
		spec package: 'Glamour-Morphic-Widgets'.
		spec package: 'Glamour-Morphic-Renderer'.
		spec package: 'Glamour-Morphic-Pager'.
		spec package: 'Glamour-Tests-Morphic'.
		spec package: 'Glamour-Rubric-Presentations'.
		spec package: 'Glamour-FastTable'.
		spec package: 'Glamour-SpecIntegration'.
	
		spec package: 'GT-Inspector'.
		spec package: 'GT-InspectorExtensions-Core'.

		spec package: 'GT-Playground'.

		spec package: 'System-Identification'.
		spec package: 'GT-EventRecorder'.

		spec package: 'GT-Spotter'.
		spec package: 'GT-SpotterExtensions-Core'.

		spec package: 'Tool-Catalog'.	"standalone"

		spec package: 'GT-Debugger'.
		spec package: 'GT-BytecodeDebugger'.
		spec package: 'GT-SUnitDebugger'.

		spec package: 'Tool-CriticBrowser'.

		spec package: 'Renraku'.
		spec package: 'QualityAssistant'.
		spec package: 'QualityAssistantRecording'.
		spec package: 'SUnit-Rules'.	

		spec package: 'Moose-Algos-Graph'.
		spec package: 'Tool-DependencyAnalyser'.
		spec package: 'Tool-DependencyAnalyser-UI'.
		spec package: 'Tool-DependencyAnalyser-Test'.	"standalone"
		spec package: 'Tool-DependencyAnalyser-Test-Data'.	"standalone"

		spec package: 'Versionner-Core-Model'.
		spec package: 'Versionner-Core-Commands'.
		spec package: 'Versionner-Core-Announcements'.
		spec package: 'Versionner-Core-DependenciesModel'.
		spec package: 'Versionner-Spec-Browser'.
		spec package: 'Versionner-Tests-Resources'.
		spec package: 'Versionner-Tests-Core-Model'.
		spec package: 'Versionner-Tests-Core-Commands'.			"standalone"
		spec package: 'Versionner-Tests-Core-DependenciesModel'.	"standalone"
		spec package: 'Versionner-Commit'.

		spec package: 'Athens-Core'.
		spec package: 'Athens-Text'.
		spec package: 'Athens-Balloon'.
		spec package: 'Athens-Morphic'.
		spec package: 'Athens-CairoPools'.
		spec package: 'Athens-Cairo'.
		spec package: 'Athens-Examples'.

		spec package: 'NautilusGroupAutoBuilder'.
		spec package: 'Kernel-Rules'.
		spec package: 'SmartSuggestions'.
		
		spec package: 'MonticelloMocks'.
		spec package: 'NodeNavigation'.
		spec package: 'OpalTools'.

		spec package: 'OSWindow-Core'.
		spec package: 'OSWindow-SDL2'.
		spec package: 'OSWindow-VM'.
				
		spec package: 'System-CommandLine-TextSupport'.
		spec package: 'GeneralRules'.
		spec package: 'ReflectionMirrors-Primitives'.
		spec package: 'FuzzyMatcher'.
		spec package: 'Versionner-Nautilus'.
		spec package: 'STON-Text support'.
		
		"SortFunctions (standalone)"
		spec package: 'SortFunctions-Core'.
		spec package: 'SortFunctions-Tests'.

		"standalone packages"
		spec package: 'Announcements-Help'.
		spec package: 'Announcements-Tests-Core'.
		spec package: 'Balloon-Tests'.
		spec package: 'BlueInk-Extras'.
		spec package: 'BlueInk-Tests'.
		spec package: 'Collections-DoubleLinkedListTests'.
		spec package: 'Compression-Tests'.
		spec package: 'ConfigurationCommandLineHandler-Tests'.
		spec package: 'ContributingToTheCatalogHelp'.
		spec package: 'Debugger-Tests'.
		spec package: 'EmbeddedFreeType-Tests'.
		spec package: 'FileSystem-Tests-Disk'.
		spec package: 'FileSystem-Tests-Memory'.
		spec package: 'Files-Tests'.
		spec package: 'Flashback-Decompiler-Tests'.
		spec package: 'FontInfrastructure-Tests'.
		spec package: 'FreeType-Tests'.
		spec package: 'FuzzyMatcher-Tests'.
		spec package: 'GT-EventRecorder-Tests'.
		spec package: 'GT-Spotter-EventRecorder'.
		spec package: 'GT-Tests-Debugger'.
		spec package: 'GT-Tests-Inspector'.
		spec package: 'GT-Tests-Playground'.
		spec package: 'GT-Tests-Spotter'.
		spec package: 'Glamour-Morphic-Brick-Tests'.
		spec package: 'Glamour-Tests-Rubric'.
		spec package: 'Gofer-UI'.	
		spec package: 'Graphics-Tests'.
		spec package: 'HelpSystem-Tests'.
		spec package: 'HowToContributeHelp'.
		spec package: 'ImportingResource-Help'.
		spec package: 'IssueTracking-Tests'.
		spec package: 'JobsTests'.
		spec package: 'Kernel-Tests-Rules'.
		spec package: 'Keymapping-Tests'.
		spec package: 'Manifest-Tests'.
		spec package: 'MessageBrowserRefactoringAddition'.
		spec package: 'Morphic-Tests'.
		spec package: 'Multilingual-Tests'.
		spec package: 'NECompletion-Tests'.
		spec package: 'Nautilus-Tests'.
		spec package: 'NodeNavigation-Tests'.
		spec package: 'OSWindow-SDL2-Examples'.
		spec package: 'OSWindow-Tests'.
		spec package: 'Pharo-Help'.
		spec package: 'PharoDocComment'.
		spec package: 'Polymorph-Widgets-Rules'.
		spec package: 'ProfStef-Tests'.
		spec package: 'QualityAssistant-Test'.
		spec package: 'Random-Tests'.
		spec package: 'Refactoring-Help'.
		spec package: 'Refactoring-Tests-Changes'.
		spec package: 'Refactoring-Tests-Critics'.
		spec package: 'Refactoring-Tests-Environment'.
		spec package: 'ReflectionMirrors-Primitives-Tests'.
		spec package: 'Reflectivity-Examples'.
		spec package: 'Reflectivity-Tools-Tests'.
		spec package: 'Regex-Help'.
		spec package: 'Regex-Tests-Core'.
		spec package: 'Renraku-Help'.
		spec package: 'Renraku-Test'.
		spec package: 'Ring-Tests-Monticello'.
		spec package: 'Rubric-Rules'.
		spec package: 'Rubric-Tests'.
		spec package: 'ScriptingExtensions-Tests'.
		spec package: 'STON-Tests'.
		spec package: 'SUnit-Tests'.
		spec package: 'Shout-Tests'.
		spec package: 'Slot-Tests'.
		spec package: 'SmartSuggestions-Tests'.
		spec package: 'Spec-Help'.	
		spec package: 'Spec-Tests'.
		spec package: 'System-CachingTests'.
		spec package: 'System-History-Tests'.
		spec package: 'System-Localization-Tests'.
		spec package: 'System-SessionManager-Tests'.
		spec package: 'System-Settings-Rules'.
		spec package: 'System-Settings-Tests'.
		spec package: 'System-Support-Rules'.
		spec package: 'System-SupportTests'.
		spec package: 'Text-Edition-Tests'.
		spec package: 'Tool-FileList-Tests'.
		spec package: 'Tools-Test'.	
		spec package: 'WorldMenuHelp'.	
		spec package: 'WriteBarrierTests'.
		spec package: 'Zinc-Character-Encoding-Tests'.
		spec package: 'Zinc-Resource-Meta-FileSystem'.	
		spec package: 'Zinc-Tests'.
		spec package: 'TravisIntegrationHelp'.
		spec package: 'System-Identification-Tests'.

		spec package: 'BaselineOfPharoBootstrap'.

      self groups: spec.
	].