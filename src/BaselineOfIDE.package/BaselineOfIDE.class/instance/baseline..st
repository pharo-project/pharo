baseline
baseline: spec 
    
	<baseline>

 	| repository | 
	
	repository := 'filetree://./pharo-core/src'.    
       
	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.
		
		spec baseline: 'BasicTools' with: [spec repository: repository].

		spec package: 'ReleaseTests'.
		spec package: 'Refactoring-Critics'.
		spec package: 'NautilusRefactoring'.

		spec package: 'Ombu'.
		spec package: 'OmbuTests'.
		spec package: 'Epicea'.
		spec package: 'EpiceaTests'.
		spec package: 'EpiceaBrowsers'.
		spec package: 'EpiceaBrowsersTests'.
		
		spec package: 'Shout'.
		spec package: 'Reflectivity-Tools'.
		spec package: 'Reflectivity-Examples'.

		spec package: 'Rubric-Styling'.

		spec package: 'FuelTools-Debugger'.
		
		spec package: 'Deprecated60'.
		spec package: 'Flashback-Decompiler'.
		spec package: 'BlueInk-Core'.
		
		spec package: 'Collections-Tests'.
		spec package: 'Text-Tests'.
		spec package: 'Network-Tests'.
		spec package: 'Tests'.
		spec package: 'Ring-Tests-Kernel'.
		spec package: 'AST-Tests-Core'.
		spec package: 'Announcements-Tests-Core'.
		spec package: 'Balloon-Tests'.
		spec package: 'BlueInk-Tests'.
		spec package: 'Compression-Tests'.
		spec package: 'ConfigurationCommandLineHandler-Tests'.
		spec package: 'Debugger-Tests'.
		spec package: 'EmbeddedFreeType-Tests'.
		spec package: 'FileSystem-Tests-Core'.
		spec package: 'FileSystem-Tests-Disk'.
		spec package: 'FileSystem-Tests-Memory'.
		spec package: 'Files-Tests'.
		spec package: 'Flashback-Decompiler-Tests'.
		spec package: 'FontInfrastructure-Tests'.
		spec package: 'FreeType-Tests'.
		spec package: 'FuelTests'.
		spec package: 'Gofer-Tests'.
		spec package: 'Graphics-Tests'.
		spec package: 'HelpSystem-Tests'.
		spec package: 'IssueTracking-Tests'.
		spec package: 'JobsTests'.
		spec package: 'Kernel-Tests'.
		spec package: 'Keymapping-Tests'.
		spec package: 'Manifest-Core'.
		spec package: 'Manifest-Resources-Tests'.
		spec package: 'Manifest-Tests'.
		spec package: 'Monticello-Tests'.
		spec package: 'Morphic-Tests'.
		spec package: 'Multilingual-Tests'.
		spec package: 'NECompletion-Tests'.
		spec package: 'Nautilus-Tests'.
		spec package: 'OpalCompiler-Tests'.
		spec package: 'ProfStef-Tests'.
		spec package: 'Random-Tests'.
		spec package: 'Refactoring-Tests-Changes'.
		spec package: 'Refactoring-Tests-Core'.
		spec package: 'Refactoring-Tests-Critics'.
		spec package: 'Refactoring-Tests-Environment'.
		spec package: 'Reflectivity-Tests'.
		spec package: 'Regex-Tests-Core'.
		spec package: 'ReleaseTests'.
		spec package: 'Ring-Tests-Containers'.
		spec package: 'Ring-Tests-Kernel'.
		spec package: 'Ring-Tests-Monticello'.
		spec package: 'SUnit-Tests'.
		spec package: 'ScriptLoader-Tests'.
		spec package: 'Shout-Tests'.
		spec package: 'Slot-Tests'.
		spec package: 'Spec-Tests'.
		spec package: 'System-CachingTests'.
		spec package: 'System-History-Tests'.
		spec package: 'System-Localization-Tests'.
		spec package: 'System-Settings-Tests'.
		spec package: 'System-SupportTests'.
		spec package: 'Text-Edition-Tests'.
		spec package: 'Tool-FileList-Tests'.
		
		spec package: 'UpdateStreamer-Tests'.
		spec package: 'Reflectivity-Tools-Tests'.
		spec package: 'System-Serial Port'.
		
		spec package: 'Zinc-Character-Encoding-Tests'.
		spec package: 'Zinc-Resource-Meta-Tests'.
		spec package: 'Zinc-Tests'.

		spec package: 'Zodiac-Tests'.	
		spec package: 'Zodiac-Extra'.	
		spec package: 'Zinc-Zodiac'.	
		spec package: 'Zinc-Resource-Meta-FileSystem'.	
			
		spec package: 'Gofer-UI'.	
		spec package: 'Tools-Test'.	
		spec package: 'Metacello-GitBasedRepository'.	
		spec package: 'WorldMenuHelp'.	
		spec package: 'WriteBarrierTests'.	
		spec package: 'SUnit-Rules'.	

		spec package: 'BaselineOfMetacello'.
		spec package: 'Metacello-Bitbucket'.
		spec package: 'Metacello-TestsMCResources'.
		spec package: 'Metacello-TestsCore'.
		spec package: 'Metacello-TestsCommonMC'.
		spec package: 'Metacello-TestsMC'.
		spec package: 'Metacello-TestsMCCore'.
		spec package: 'Metacello-TestsPlatform'.
		spec package: 'Metacello-TestsReference'.

		spec package: 'RPackage-Tests'.

		spec package: 'System-SessionManager-Tests'.

		spec package: 'BaselineOfFileTree'.

		spec package: 'Glamour-Announcements'.
		spec package: 'Glamour-Helpers'.
		spec package: 'Glamour-Core'.
		spec package: 'Glamour-Presentations'.
		spec package: 'Glamour-Browsers'.
		spec package: 'Glamour-Examples'.
		spec package: 'Glamour-Morphic-Brick'.
		spec package: 'Glamour-Morphic-Brick-Tests'.
		spec package: 'Glamour-Tests-Core'.
		spec package: 'Glamour-Tests-Resources'.
		spec package: 'Glamour-Morphic-Theme'.
		spec package: 'Glamour-Morphic-Widgets'.
		spec package: 'Glamour-Morphic-Renderer'.
		spec package: 'Glamour-Morphic-Pager'.
		spec package: 'Glamour-Tests-Morphic'.
		spec package: 'Glamour-Rubric-Presentations'.
		spec package: 'Glamour-Tests-Rubric'.
		spec package: 'Glamour-FastTable'.
		spec package: 'Glamour-SpecIntegration'.
	
		spec package: 'GT-Inspector'.
		spec package: 'GT-InspectorExtensions-Core'.
		spec package: 'GT-Tests-Inspector'.

		spec package: 'GT-Playground'.
		spec package: 'GT-Tests-Playground'.

		spec package: 'GT-EventRecorder'.
		spec package: 'GT-EventRecorder-Tests'.

		spec package: 'GT-Spotter'.
		spec package: 'GT-Spotter-EventRecorder'.
		spec package: 'GT-SpotterExtensions-Core'.
		spec package: 'GT-Tests-Spotter'.

		spec package: 'Tool-Catalog'.

		spec package: 'GT-Debugger'.
		spec package: 'GT-BytecodeDebugger'.
		spec package: 'GT-SUnitDebugger'.
		spec package: 'GT-Tests-Debugger'.

		spec package: 'Tool-CriticBrowser'.

		spec package: 'Renraku'.
		spec package: 'Renraku-Test'.
		spec package: 'Renraku-Help'.
		spec package: 'QualityAssistant'.
		spec package: 'QualityAssistantRecording'.
		spec package: 'QualityAssistant-Test'.

		spec package: 'Moose-Algos-Graph'.
		spec package: 'Tool-DependencyAnalyser'.
		spec package: 'Tool-DependencyAnalyser-Test'.
		spec package: 'Tool-DependencyAnalyser-UI'.

		spec package: 'ShoreLine-Report-Core'.
		spec package: 'ShoreLine-Report-UI'.
		spec package: 'ShoreLine-Report-Settings'.
		
		spec package: 'Versionner-Core-Model'.
		spec package: 'Versionner-Core-Commands'.
		spec package: 'Versionner-Core-Announcements'.
		spec package: 'Versionner-Core-DependenciesModel'.
		spec package: 'Versionner-Spec-Browser'.
		spec package: 'Versionner-Tests-Resources'.
		spec package: 'Versionner-Tests-Core-Model'.
		spec package: 'Versionner-Tests-Core-Commands'.
		spec package: 'Versionner-Tests-Core-DependenciesModel'.
		spec package: 'Versionner-Commit'.

		spec package: 'Athens-Core'.
		spec package: 'Athens-Text'.
		spec package: 'Athens-Balloon'.
		spec package: 'Athens-Morphic'.
		spec package: 'Athens-CairoPools'.
		spec package: 'Athens-Cairo'.
		spec package: 'Athens-Examples'.

		spec package: 'TxText-Model'.
		spec package: 'TxText-Athens'.
		spec package: 'TxText-Styler'.
		spec package: 'TxTextTests-Model'.
		spec package: 'TxText-AthensTests'.
		spec package: 'Tool-TxWorkspace'.

		spec package: 'NautilusGroupAutoBuilder'.
		spec package: 'Kernel-Rules'.
		spec package: 'SmartSuggestions'.
		spec package: 'SmartSuggestions-Tests'.
		
		spec package: 'MonticelloMocks'.
		spec package: 'NodeNavigation'.
		spec package: 'OpalTools'.
		spec package: 'Rubric-Rules'.

		spec package: 'OSWindow-Core'.
		spec package: 'OSWindow-SDL2'.
		spec package: 'OSWindow-VM'.
		spec package: 'OSWindow-SDL2-Examples'.
		spec package: 'OSWindow-Tests'.
	].