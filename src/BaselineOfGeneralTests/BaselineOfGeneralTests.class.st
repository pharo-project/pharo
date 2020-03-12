Class {
	#name : #BaselineOfGeneralTests,
	#superclass : #BaselineOf,
	#category : #BaselineOfGeneralTests
}

{ #category : #baselines }
BaselineOfGeneralTests >> baseline: spec [
	<baseline>
	"Needs to be re-sorted"
	
	spec for: #'common' do: [
		spec 
			package: 'Debugging-Utils-Tests';
			package: 'Tests';
			package: 'AST-Core-Tests';			
			package: 'RPackage-Tests';
			package: 'Monticello-Tests';	"required by MonticelloMocks"
			package: 'System-Installers-Tests' ;
			package: 'MonticelloMocks';
			package: 'Fuel-Tests-Core';
			package: 'Balloon-Tests';
			package: 'Collections-DoubleLinkedList-Tests';
			package: 'Collections-Arithmetic-Tests';
			package: 'ConfigurationCommandLineHandler-Tests';
			package: 'MetacelloCommandLineHandler-Tests';
			package: 'Debugger-Tests';
			package: 'EmbeddedFreeType-Tests';
			
			package: 'Ombu-Tests';
			package: 'Epicea-Tests';
			package: 'Hiedra-Tests';
			package: 'EpiceaBrowsers-Tests';
			
			package: 'FileSystem-Tests-Memory';
			package: 'Fonts-Infrastructure-Tests';
			package: 'FreeType-Tests';
			package: 'FreeType-Help';

			package: 'Graphics-Tests';
			package: 'HelpSystem-Tests';
			package: 'IssueTracking-Tests';
			package: 'Keymapping-Tests';

			package: 'Morphic-Tests';
			
			package: 'Morphic-Widgets-FastTable-Tests';
			package: 'NECompletion-Tests';
			package: 'NewValueHolder-Tests';
			package: 'ProfStef-Tests';
			package: 'Reflectivity-Tools-Tests';
			package: 'Regex-Core-Tests';
			package: 'Ring-Definitions-Tests-Monticello';
			package: 'Rubric-Tests';
			package: 'ScriptingExtensions-Tests';
			package: 'STON-Tests';
			package: 'Spec-Tests';
			package: 'System-Caching-Tests';
			package: 'System-Hashing-Tests';
			package: 'System-History-Tests';
			package: 'System-Localization-Tests';
			package: 'System-SessionManager-Tests';
			package: 'System-Settings-Tests';
			package: 'System-Support-Tests';
			package: 'Text-Edition-Tests';
			package: 'Tool-FileList-Tests';
			package: 'Tools-Test';
			package: 'Zinc-Resource-Meta-FileSystem';
			package: 'Zinc-Resource-Meta-Tests';
			package: 'Zinc-HTTP-Examples';
			package: 'Zinc-Tests';	"requires Zinc-Resource-Meta-Tests"
			package: 'System-Identification-Tests'	;	
			package: 'System-DependenciesTests';
			package: 'System-BasicCommandLineHandler-Tests';
			package: 'Transcript-NonInteractive-Tests';
			package: 'PharoDocComment-Tests'	"standalone" ]
]
