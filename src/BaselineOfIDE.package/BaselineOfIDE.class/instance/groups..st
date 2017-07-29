baseline
groups: spec 
    
spec group: 'Mandatory-Group' with: #(
	'FuelTools-Debugger'	"required by GT-Debugger HudsonBuildTools20"
	'Network-Tests'		"required by Multilingual-Tests"
	'Gofer-Tests'		"required by EpiceaTests Metacello-TestsMC, depends on 'Metacello-Tests-Group', cleanup needed?"
). 

spec group: 'Optional-Group' with: #(
	'Athens-Group' "dependency"

	'PharoDocComment'	"standalone"
	'STON-Text support'	"standalone?"
	'System-CommandLine-TextSupport'	"standalone?"
	'System-Serial Port'	"standalone?"
	'Gofer-UI'	"standalone"
	'MessageBrowserRefactoringAddition' 	"standalone"
).	
	
spec group: 'NodeNavigation-Group' with: #(
	'NodeNavigation'	"standalone?"
	'NodeNavigation-Tests'	
).					

spec group: 'Epicea-Core-Group' with: #(
	'Athens-Group' "dependency of Hiedra"
	'Ombu'
	'Epicea'
	'Hiedra'
	'EpiceaBrowsers').
spec group: 'Epicea-Tests-Group' with: #(
	'OmbuTests'
	'EpiceaTests'
	'HiedraTests'
	'EpiceaBrowsersTests'
).	
spec group: 'Epicea-Group' with: #(
	'Epicea-Core-Group' 
	'Epicea-Tests-Group'
).	

spec group: 'General-Help-Group' with: #(
	'Announcements-Help'
	'HowToContributeHelp'
	'ImportingResource-Help'
	'Pharo-Help'
	'Regex-Help'
	'Spec-Help'	
	'TravisIntegrationHelp'	
	'WorldMenuHelp'
).	

spec group: 'General-Tests-Group' with: #(
	'Kernel-Tests-Group' "dependency"
	'Monticello-Tests'	"required by MonticelloMocks"
	'MonticelloMocks'
	'FuelTests'
	'Balloon-Tests'
	'Collections-DoubleLinkedListTests'
	'ConfigurationCommandLineHandler-Tests'
	'Debugger-Tests'
	'EmbeddedFreeType-Tests'
	'FileSystem-Tests-Memory'
	'FontInfrastructure-Tests'
	'FreeType-Tests'

	'Gofer-UI'	
	'Graphics-Tests'
	'HelpSystem-Tests'
	'IssueTracking-Tests'
	'Keymapping-Tests'

	'MessageBrowserRefactoringAddition'
	'Morphic-Tests'
	'NECompletion-Tests'
	'Nautilus-Tests'
	'OmbuTests'
	'ProfStef-Tests'

	'Reflectivity-Tools-Tests'
	'Regex-Tests-Core'
	'Ring-Tests-Monticello'
	'Rubric-Tests'
	'ScriptingExtensions-Tests'
	'STON-Tests'
	'SUnit-Tests'
	'Slot-Tests'
	'Spec-Tests'
	'System-CachingTests'
	'System-History-Tests'
	'System-Localization-Tests'
	'System-SessionManager-Tests'
	'System-Settings-Tests'
	'System-SupportTests'
	'Text-Edition-Tests'
	'Tool-FileList-Tests'
	'Tools-Test'	
	'WriteBarrierTests'
	'Zinc-Resource-Meta-FileSystem'	
	'Zinc-Resource-Meta-Tests'
	'Zinc-Tests'	"requires Zinc-Resource-Meta-Tests"
	'System-Identification-Tests'		
).	

spec group: 'FuzzyMatcher-Core-Group' with: #(
	'FuzzyMatcher'
).
spec group: 'FuzzyMatcher-Tests-Group' with: #(
	'FuzzyMatcher-Tests'
).
spec group: 'FuzzyMatcher-Group' with: #(
	'FuzzyMatcher-Core-Group'
	'FuzzyMatcher-Tests-Group'
).

spec group: 'Zodiac-Extras-Group' with: #(
	'Zodiac-Extra'
	'Zinc-Zodiac'
).
spec group: 'Zodiac-Tests-Group' with: #(
	'Zodiac-Tests'
).
spec group: 'Zodiac-Group' with: #(
	'Zodiac-Extras-Group'
	'Zodiac-Tests-Group'
).

spec group: 'ReflectionMirrors-Core-Group' with: #(
	'ReflectionMirrors-Primitives'
).
spec group: 'ReflectionMirrors-Tests-Group' with: #(
	'ReflectionMirrors-Primitives-Tests'
).
spec group: 'ReflectionMirrors-Group' with: #(
	'ReflectionMirrors-Core-Group'
	'ReflectionMirrors-Tests-Group'
).

spec group: 'Reflectivity-Group' with: #(
	'Reflectivity-Tests'	"required by Reflectivity-Examples"
	'Reflectivity-Examples'
).

spec group: 'SmartSuggestions-Core-Group' with: #(
	"depends on 'Glamour-Core-Group', circular dependency, needs fix"
	'SmartSuggestions'
).
spec group: 'SmartSuggestions-Tests-Group' with: #(
	'SmartSuggestions-Tests'
).
spec group: 'SmartSuggestions-Group' with: #(
	'SmartSuggestions-Core-Group'
	'SmartSuggestions-Tests-Group'
).

"spec group: 'Deprecated-Group' with: #(
	'Deprecated70'
)."

spec group: 'Manifest-Core-Group' with: #(
	'Manifest-Core'
).
spec group: 'Manifest-Tests-Group' with: #(
	'Kernel-Tests-Group' "dependency"
	'Manifest-Resources-Tests'
	'Manifest-Tests'	"depends on QA, needs cleanup"
).
spec group: 'Manifest-Group' with: #(
	'Manifest-Core-Group'
	'Manifest-Tests-Group'
).

spec group: 'Flashback-Core-Group' with: #(
	'Flashback-Decompiler'
).
spec group: 'Flashback-Tests-Group' with: #(
	'Flashback-Decompiler-Tests'
).
spec group: 'Flashback-Group' with: #(
	'Flashback-Core-Group'
	'Flashback-Tests-Group'
).

spec group: 'BlueInk-Core-Group' with: #(
	'BlueInk-Core'
	'BlueInk-Extras'
).
spec group: 'BlueInk-Tests-Group' with: #(
	'Kernel-Tests-Group' "dependency"
	'BlueInk-Tests'	
).
spec group: 'BlueInk-Group' with: #(
	'BlueInk-Core-Group'
	'BlueInk-Tests-Group'
).

spec group: 'Shout-Core-Group' with: #(
	'Shout'		"'Polymorph-Widgets' and 'NECompletion' depend on it, fix needed?"
	'Rubric-Styling'	"requires 'Shout'"      
).
spec group: 'Shout-Tests-Group' with: #(
	'Shout-Tests'
).
spec group: 'Shout-Group' with: #(
	'Shout-Core-Group'
	'Shout-Tests-Group'
).

spec group: 'MetacelloExtras-Core' with: #(
	'Metacello-GitBasedRepository'	
	'Metacello-Bitbucket'	
	"'Metacello-GitHub'"
).
spec group: 'Metacello-Tests-Group' with: #(
	'Metacello-TestsMCResources'
	'Metacello-TestsCore'
	'Metacello-TestsCommonMC'
	'Metacello-TestsMC'	
	'Metacello-TestsMCCore'	
	'Metacello-TestsPlatform'	
	'Metacello-TestsReference'	
).

spec group: 'Refactoring-Rules-Group' with: #(
	'QualityAssistant-Core-Group' "dependency"
	'SUnit-Rules'	
	'Kernel-Tests-Rules'
	'Polymorph-Widgets-Rules'
	'System-Settings-Rules'
	'System-Support-Rules'
	'Rubric-Rules'
).
spec group: 'Refactoring-Tests-Group' with: #(
	'Refactoring-Tests-Core'		"'Kernel-Tests' depend on it, cleanup needed!"
	'Refactoring-Tests-Changes'
	'Refactoring-Tests-Critics'
	'Refactoring-Tests-Environment'
).
spec group: 'Refactoring-Help-Group' with: #(
	'Refactoring-Help'

).
spec group: 'Refactoring-Group' with: #(
	'Refactoring-Rules-Group'
	'Refactoring-Tests-Group'
	'Refactoring-Help-Group' 
).

spec group: 'Catalog-Core-Group' with: #(
	'Glamour-Core-Group'	"dependency"
	'GT-Core-Group'	"dependency"
	'Tool-Catalog'
).
spec group: 'Catalog-Help-Group' with: #(
	'ContributingToTheCatalogHelp'
).
spec group: 'Catalog-Group' with: #(
	'Catalog-Core-Group'
	'Catalog-Help-Group'
).

spec group: 'SortFunctions-Group' with: #(
	'SortFunctions-Core'
	'SortFunctions-Tests'
).

spec group: 'QualityAssistant-Core-Group' with: #(
	"'Manifest-Tests' 'Polymorph-Widgets-Rules' depend on it, needs cleanup"
	'Manifest-Core-Group' "dependency"
	'Glamour-Core-Group'	"dependency"
	'DependencyAnalyser-Core-Group' "dependency"
	'GT-Core-Group'	"dependency"
	'Renraku'		
	'QualityAssistant'
	'QualityAssistantRecording'
	'Tool-CriticBrowser'
).
spec group: 'QualityAssistant-Tests-Group' with: #(
	'QualityAssistant-Test'
	'Renraku-Test'
).
spec group: 'QualityAssistant-Help-Group' with: #(
	'Renraku-Help'
).
spec group: 'QualityAssistant-Group' with: #(
	'QualityAssistant-Core-Group'
	'QualityAssistant-Tests-Group'
	'QualityAssistant-Help-Group'
).

spec group: 'Glamour-Core-Group' with: #(
	'Shout-Core-Group'  "dependecy"
	"circular dependency on 'Glamour-Tests-Group', 'GT-Core-Group'"
	'Athens-Group' "dependency"
	'Glamour-Announcements'
	'Glamour-Helpers'
	'Glamour-Core'
	'Glamour-Presentations'
	'Glamour-Browsers'
	'Glamour-Examples'
	'Glamour-Morphic-Brick'
	'Glamour-Morphic-Theme'
	'Glamour-Morphic-Widgets'
	'Glamour-Morphic-Renderer'
	'Glamour-Morphic-Pager'
	'Glamour-Rubric-Presentations'	"requires Shout"
	'Glamour-FastTable'	
	'Glamour-SpecIntegration'
).
spec group: 'Glamour-Tests-Group' with: #(
	'Glamour-Tests-Core'
	'Glamour-Tests-Resources'
	'Glamour-Tests-Morphic'
	'Glamour-Morphic-Brick-Tests'
	'Glamour-Tests-Rubric'
).
spec group: 'Glamour-Group' with: #(
	'Glamour-Core-Group'
	'Glamour-Tests-Group'
).

spec group: 'GT-Core-Group' with: #(
	'SmartSuggestions-Group' "dependency"
	'Glamour-Core-Group'	"dependency"
	"'Morphic-Core' 'System-Settings' is dependent on this group, needs clenup"
	'OpalTools'
	'GT-Inspector'
	'GT-InspectorExtensions-Core'
	'GT-Playground'
	'System-Identification'
	'GT-EventRecorder'
	'GT-Spotter'
	'GT-SpotterExtensions-Core'
	'GT-Spotter-EventRecorder'
	'GT-Debugger'
	'GT-BytecodeDebugger'
	'GT-SUnitDebugger'
).
spec group: 'GT-Tests-Group' with: #(
	'GT-EventRecorder-Tests'
	'GT-Tests-Debugger'
	'GT-Tests-Inspector'
	'GT-Tests-Playground'
	'GT-Tests-Spotter'
).
spec group: 'GT-Group' with: #(
	'GT-Core-Group'
	'GT-Tests-Group'
).

spec group: 'DependencyAnalyser-Core-Group' with: #(
	'Manifest-Core-Group' "dependency"
	'Moose-Algos-Graph'
	'Tool-DependencyAnalyser'
	'Tool-DependencyAnalyser-UI'
).
spec group: 'DependencyAnalyser-Tests-Group' with: #(
	'Tool-DependencyAnalyser-Test'	
	'Tool-DependencyAnalyser-Test-Data'	
).
spec group: 'DependencyAnalyser-Group' with: #(
	'DependencyAnalyser-Core-Group'
	'DependencyAnalyser-Tests-Group'
).		

spec group: 'Athens-Group' with: #(
	"dependent on Rubric, cleanup?"
	'Athens-Core'
	'Athens-Text'
	'Athens-Balloon'
	'Athens-Morphic'
	'Athens-CairoPools'
	'Athens-Cairo'
	'Athens-Examples'
).		

spec group: 'OSWindow-Core-Group' with: #(
	'Glamour-Core-Group'	"dependency, cleanup needed"
	'GT-Core-Group'	"dependency, cleanup needed"
	'Athens-Group' "dependency"
	'OSWindow-Core'
	'OSWindow-SDL2'
	'OSWindow-VM'
	'OSWindow-SDL2-Examples'
).	
spec group: 'OSWindow-Tests-Group' with: #(
	'OSWindow-Tests'
).
spec group: 'OSWindow-Group' with: #(
	'OSWindow-Core-Group'
	'OSWindow-Tests-Group'
).


spec group: 'Versionner-Core-Group' with: #(
	'Glamour-Core-Group'	"dependency, cleanup needed"
	'Versionner-Core-Model'
	'Versionner-Core-Commands'
	'Versionner-Core-Announcements'
	'Versionner-Core-DependenciesModel'
	'Versionner-Spec-Browser'
	'Versionner-Commit'
).
spec group: 'Versionner-Tests-Group' with: #(
	'Kernel-Tests-Group' "dependency"
	'Versionner-Tests-Resources'
	'Versionner-Tests-Core-Model'
	'Versionner-Tests-Core-Commands'			
	'Versionner-Tests-Core-DependenciesModel'	
).
spec group: 'Versionner-Group' with: #(
	'Versionner-Core-Group'
	'Versionner-Tests-Group'
	'NautilusGroupAutoBuilder'	"required by Versionner-Nautilus"
	'Versionner-Nautilus'	
).

spec group: 'Kernel-Tests-Group' with: #(
	'BlueInk-Core-Group' "dependency"
	'RPackage-Tests' 
	'Collections-Tests'
	'Kernel-Tests' 
	'AST-Tests-Core' "requires BlueInk-Core"
	'Announcements-Tests-Core'
	'Compression-Tests' 
	'FileSystem-Tests-Core' 
	'FileSystem-Tests-Disk'
	'Files-Tests' 
	'JobsTests' 
	'Multilingual-Tests'
	'OpalCompiler-Tests' 
	'Random-Tests'
	"'ReleaseTests'"
	'Ring-Tests-Kernel'
	'Ring-Tests-Containers' 
	'Tests' 
	'Zinc-Character-Encoding-Tests' 
	'FileSystem-Tests-Memory'

	'Text-Tests'		"required by Multilingual-Tests"
).

spec group: 'GT&QA-Group' with: #(
	'GeneralRules'		"depends on QA, needs cleanup"
	'Kernel-Rules'		"depends on QA, DependencyAnalyzer, needs cleanup"

	'ReleaseTests' 		"required by Manifest-Core Refactoring-Critics"
	'Reflectivity-Tools'	"required by Manifest-Core Refactoring-Critics"

	'NautilusRefactoring'	"required by Epicea, SmartSuggestions"
	'Refactoring-Critics'	"depends on QA, needs cleanup"

	'Glamour-Group'
	'GT-Group'

	'Manifest-Group'
	'DependencyAnalyser-Group'
	'QualityAssistant-Group'
	'Reflectivity-Group'	
).		


spec group: #default with: #(
	'BasicTools'	"dependency"

	'Mandatory-Group'

	'Athens-Group'
	'Kernel-Tests-Group'
	'BlueInk-Group'

	'Shout-Group'

	'System-Localization'

	"standalone:" 
	'MetacelloExtras-Core'
	'Metacello-Tests-Group'		 
	'General-Help-Group'
	'ReflectionMirrors-Group'
	"'Deprecated-Group'"
	'Zodiac-Group'
	'FuzzyMatcher-Group'
	'Flashback-Group'

	'GT&QA-Group'

	"standalone but dependent:"
	'OSWindow-Group'
	'Refactoring-Group'
	'Catalog-Group'
	'Epicea-Group'
	'Optional-Group'
	'NodeNavigation-Group'
	'Versionner-Group'
	'General-Tests-Group'
	
	'SortFunctions-Group'	
	
	'BaselineOfPharoBootstrap'
).