Class {
	#name : 'BaselineOfGeneralTests',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfGeneralTests',
	#package : 'BaselineOfGeneralTests'
}

{ #category : 'baselines' }
BaselineOfGeneralTests >> baseline: spec [
	"Needs to be re-sorted"

	<baseline>
	| repository |
	repository := self packageRepositoryURL.
	spec for: #'common' do: [
		spec
			package: 'Debugging-Utils-Tests';
			package: 'NumberParser-Tests';
			package: 'AST-Core-Tests';
			package: 'RPackage-Tests';
			package: 'Monticello-Tests';	"required by MonticelloMocks"
			package: 'System-Installers-Tests' ;
			package: 'MonticelloMocks';
			package: 'Fuel-Core-Tests';
			package: 'FormCanvas-Tests';
			package: 'Collections-DoubleLinkedList-Tests';
			package: 'Collections-Arithmetic-Tests';
			package: 'MetacelloCommandLineHandler-Tests';
			package: 'Debugger-Model-Tests';
			package: 'Debugger-Oups-Tests';
			package: 'EmbeddedFreeType-Tests';


			baseline: 'Epicea' with: [
				spec
					repository: repository;
					loads: #Tests ];

			package: 'FileSystem-Memory-Tests';
			package: 'Fonts-Infrastructure-Tests';

			package: 'Graphics-Tests';

			package: 'Morphic-Tests';

			package: 'Morphic-Widgets-FastTable-Tests';
			package: 'NECompletion-Tests';
			package: 'ProfStef-Tests';
			package: 'Regex-Core-Tests';
			package: 'Ring-Definitions-Monticello-Tests';
			package: 'Rubric-Tests';
			package: 'ScriptingExtensions-Tests';
			package: 'STON-Tests';
			package: 'System-Caching-Tests';
			package: 'System-Clipboard-Tests';
			package: 'System-Hashing-Tests';
			package: 'System-History-Tests';
			package: 'System-Localization-Tests';
			package: 'System-SessionManager-Tests';
			package: 'System-Settings-Tests';
			package: 'System-Support-Tests';
			package: 'Text-Diff-Tests';
			package: 'Tool-ExternalBrowser-Tests';
			package: 'Tool-FileList-Tests';
			package: 'Tool-Finder-Tests';
			package: 'Tool-Profilers-Tests';
			package: 'Tool-ImageCleaner-Tests';
			package: 'Tools-CodeNavigation-Tests';
			package: 'Tools-Tests';
			package: 'Zinc-Resource-Meta-Tests';
			package: 'Zinc-HTTP-Examples';
			package: 'Zinc-Tests';	"requires Zinc-Resource-Meta-Tests"
			package: 'System-Identification-Tests'	;
			package: 'System-DependenciesTests';
			package: 'System-BasicCommandLineHandler-Tests';
			package: 'Transcript-NonInteractive-Tests';
			package: 'PharoDocComment-Tests'	"standalone" ]
]
