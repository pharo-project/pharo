Class {
	#name : #BaselineOfPillar,
	#superclass : #BaselineOf,
	#category : #BaselineOfPillar
}

{ #category : #baselines }
BaselineOfPillar >> baseline: spec [
	<baseline>
	spec
		for: #windows
		do: [ spec configuration: 'ProcessWrapper' 
				with: [ spec
					versionString: '1.2';
					repository: 'http://smalltalkhub.com/mc/hernan/ProcessWrapper/main' ].
			spec package: 'Pillar-Project' with: [ spec requires: 'ProcessWrapper' ] ].
	spec
		for: #(unix osx)
		do: [ spec baseline: 'OSSubprocess' with: [ spec repository: 'github://pharo-contributions/OSSubprocess:v1.2.1/repository' ].
			spec package: 'Pillar-Project' with: [ spec requires: 'OSSubprocess' ] ].
	
	
	spec
		for: #common
		do: [ spec blessing: #baseline.
			spec
				baseline: 'ContainersPropertyEnvironment'
				with: [ spec repository: 'github://Ducasse/Containers-PropertyEnvironment:v1.0.0' ].
				
				
			spec
				baseline: 'PetitParser2Core' with: [
					spec
					 	repository: 'github://kursjan/petitparser2' ];
				baseline: 'Chrysal' with: [ spec repository: 'github://Ducasse/Chrysal:v1.0.0/src' ];
				baseline: 'Clap' with: [ spec repository: 'github://cdlm/clap-st:pharo-pr5761' ];
				baseline: 'Mustache'
					with: [ spec
						repository: 'github://noha/mustache:v1.0/repository';
						loads: #('Core' 'Tests') ].
			spec
				package: 'Pillar-Chrysal-Generator' with: [ spec requires: #('Chrysal') ];
				"Only needed to regenerate the pillar configuration: it depends on Chrysal and Chrysal-Runtime"
				package: 'Pillar-Cli' with: [ 
					spec requires: #( 'Clap' 'Pillar-ExporterCore' 'Pillar-Chrysal') ];
				package: 'Pillar-Chrysal' with: [ spec requires: #('Pillar-ExporterCore') ];
				"Pillar-Chrysal should only depend on Chrysal-Runtime but right now I do not know how to express that the
				Chrysal (builder) is different from Chrysal-Runtime"
				package: 'Pillar-ExporterAsciiDoc' with: [ spec requires: #('Pillar-ExporterCore') ];
				package: 'Pillar-ExporterBeamer' with: [ spec requires: #('Pillar-ExporterLaTeX') ];
				package: 'Pillar-ExporterCore' with: [ spec requires: #('Pillar-Model' 'ContainersPropertyEnvironment') ];
				package: 'Pillar-ExporterDeckJS' with: [ spec requires: #('Pillar-ExporterHTML') ];
				package: 'Pillar-ExporterEPub' with: [ spec requires: #('Pillar-ExporterCore' 'Pillar-ExporterHTML') ];
				package: 'Pillar-ExporterHTML' with: [ spec requires: #('Pillar-ExporterCore') ];
				package: 'Pillar-ExporterLaTeX' with: [ spec requires: #('Pillar-ExporterCore' 'Pillar-ExporterPillar') ];
				package: 'Pillar-ExporterMarkdown' with: [ spec requires: #('Pillar-ExporterCore' 'Pillar-ExporterHTML') ];
				package: 'Pillar-ExporterPillar' with: [ spec requires: #('Pillar-ExporterCore' 'Pillar-PetitPillar') ];
				package: 'Pillar-ExporterText' with: [ spec requires: #('Pillar-ExporterCore') ];
				package: 'Pillar-GitBook' with: [ spec requires: #('Pillar-Cli' 'markdown exporter' 'text exporter') ];
				
				package: 'Pillar-Core';
				
				package: 'Pillar-Model' with: [ spec requires: #('Pillar-Core')];
				package: 'Pillar-CodeBlockEvaluator' with: [ spec requires: #('Pillar-Core')];
				package: 'Pillar-PetitPillar' with: [ spec requires: #('PetitParser2Core' 'Pillar-Model') ];
			
				"Going from Pillar model to richText"
				package: 'Pillar-ExporterRichText' 
					with: [ spec requires: #('Pillar-Core' ) ];
				"Going from Pillar text to richText"
				package: 'Pillar-PillarTextRichTextExporter' 
					with: [ spec requires: #('Pillar-ExporterRichText' 'Pillar-PetitPillar') ];
					
				package: 'Pillar-Pharo-Tools' with: [ spec requires: #('Pillar-Model' 'All exporters') ];
				package: 'Pillar-Project' with: [ spec requires: #('Pillar-Model' 'All exporters') ];
				package: 'Pillar-BookTester' with: [ spec requires: #('Pillar-Project') ];
				package: 'Pillar-Tests-Cli' with: [ spec requires: #('Pillar-Cli') ];
				package: 'Pillar-Tests-ExporterAsciiDoc' with: [ spec requires: #('Pillar-ExporterAsciiDoc' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-ExporterBeamer' with: [ spec requires: #('Pillar-ExporterBeamer' 'Pillar-Tests-ExporterLaTeX') ];
				package: 'Pillar-Tests-ExporterCore' with: [ spec requires: #('Pillar-Tests-Model' 'Pillar-ExporterCore') ];
				package: 'Pillar-Tests-ExporterDeckJS' with: [ spec requires: #('Pillar-ExporterDeckJS' 'Pillar-Tests-ExporterHTML') ];
				package: 'Pillar-Tests-ExporterEPub' with: [ spec requires: #('Pillar-ExporterEPub' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-ExporterHTML' with: [ spec requires: #('Pillar-ExporterHTML' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-ExporterLaTeX' with: [ spec requires: #('Pillar-ExporterLaTeX' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-ExporterMarkdown' with: [ spec requires: #('Pillar-ExporterMarkdown' 'Pillar-Tests-ExporterCore' 'Pillar-Tests-ExporterHTML') ];
				package: 'Pillar-Tests-ExporterPillar' with: [ spec requires: #('Pillar-ExporterPillar' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-ExporterText' with: [ spec requires: #('Pillar-ExporterText' 'Pillar-Tests-ExporterCore') ];
				package: 'Pillar-Tests-Core' with: [ spec requires: #('Pillar-Core' 'Pillar-ExporterPillar' 'Pillar-ExporterText')];
				package: 'Pillar-Tests-Model' with: [ spec requires: #('Pillar-Model' 'Pillar-ExporterPillar' 'Pillar-ExporterText' 'Pillar-Tests-Core') ];
				
				
				"I hope that I will be able to make core test not depending on the exporter and the text exporter."
				
				package: 'Pillar-Tests-PetitPillar' with: [ spec requires: #('PetitParser2Core' 'Pillar-PetitPillar') ];
				package: 'Pillar-Tests-BookTester' with: [ spec requires: #('Pillar-BookTester') ];
				package: 'Pillar-Tests-Project' with: [ spec requires: #('Pillar-Project') ];
				package: 'Pillar-Tests-Integration' with: [ spec requires: #('Pillar-Project') ].
			
			
			
			spec
				group: 'ALL' with: #('All tests' 'All but tests');
				
				group: 'All but tests'
					with: #('All exporters' 'Pillar-Cli' 'Pillar-Pharo-Tools' 'Pillar-GitBook' 'parser' 'Pillar-Project' 'Pillar-BookTester' 'Pillar-CodeBlockEvaluator' 'Pillar-PillarTextRichTextExporter');
				
				group: 'All exporter tests'
					with:
					#('html exporter tests' 'latex exporter tests' 'beamer exporter tests' 'deckjs exporter tests' 'markdown exporter tests' 'pillar exporter tests' 'text exporter tests' 'asciidoc exporter tests' 'ePub exporter tests');
				
				group: 'All exporters'
					with:
					#('html exporter' 'rich text exporter' 'latex exporter' 'beamer exporter' 'deckjs exporter' 'markdown exporter' 'pillar exporter' 'text exporter' 'asciidoc exporter' 'ePub exporter');
				
				group: 'All tests'
					with:
					#('All exporter tests' 'Pillar-Tests-Model' 'Pillar-Tests-ExporterCore' #'Pillar-Tests-Cli' 'parser tests' 'Pillar-Tests-Project' 'Pillar-Tests-Integration');
				
				group: 'asciidoc exporter' with: #(#'Pillar-ExporterAsciiDoc');
				group: 'asciidoc exporter tests' with: #(#'Pillar-Tests-ExporterAsciiDoc' 'ePub exporter');
				group: 'beamer exporter' with: #('Pillar-ExporterBeamer');
				group: 'beamer exporter tests' with: #('Pillar-Tests-ExporterBeamer');
				
				group: 'deckjs exporter' with: #('Pillar-ExporterDeckJS');
				group: 'deckjs exporter tests' with: #('Pillar-Tests-ExporterDeckJS');
				
				group: 'ePub exporter' with: #(#'Pillar-ExporterEPub');
				group: 'ePub exporter tests' with: #(#'Pillar-Tests-ExporterEPub');
				
				"from pillar model to richtext"
				group: 'rich text exporter' with: #('Pillar-ExporterRichText');
				
				group: 'html exporter' with: #('Pillar-ExporterHTML');
				group: 'html exporter tests' with: #('Pillar-Tests-ExporterHTML');
				
				group: 'booktester' with: #('Pillar-BookTester');
				group: 'booktester tests' with: #('Pillar-Tests-BookTester');
				
				group: 'latex exporter' with: #('Pillar-ExporterLaTeX');
				group: 'latex exporter tests' with: #('Pillar-Tests-ExporterLaTeX');
				
				group: 'markdown exporter' with: #('Pillar-ExporterMarkdown' 'parser');
				group: 'markdown exporter tests' with: #('Pillar-Tests-ExporterMarkdown');
				
				group: 'parser' with: #('Pillar-PetitPillar');
				group: 'parser tests' with: #('Pillar-Tests-PetitPillar');
				
				group: 'pillar exporter' with: #('Pillar-ExporterPillar');
				group: 'pillar exporter tests' with: #('Pillar-Tests-ExporterPillar');
				
				group: 'text exporter' with: #('Pillar-ExporterText');
				group: 'text exporter tests' with: #('Pillar-Tests-ExporterText').


		"Petit Parse One packages"			
	 	spec 
   				baseline: 'PetitParser' 
   				with: [ spec repository: 'github://moosetechnology/PetitParser:v2.2.0/src'.
								spec loads: #('Tests') ] ];
		package: 'Pillar-PetitPillarOld' with: [ spec requires: #('PetitParser' 'Pillar-Model') ];
		package: 'Pillar-Tests-PetitPillarOld' with: [ spec  requires: #('PetitParser' 'Pillar-PetitPillarOld') ];
		group: 'old' with: #( 'Pillar-PetitPillarOld' 'Pillar-Tests-PetitPillarOld' ).


]

{ #category : #baselines }
BaselineOfPillar >> customProjectAttributes [
    "Edit to return a collection of any custom attributes e.g. for conditional loading: Array with: #'Condition1' with: #'Condition2.
	For more information see: http://code.google.com/p/metacello/wiki/CustomProjectAttrributes "

	| attributes |
	attributes := OrderedCollection new.
	OSPlatform current isWindows ifTrue: [ attributes add: #windows ].
	OSPlatform current isUnix    ifTrue: [ attributes add: #unix ].
	OSPlatform current isMacOS   ifTrue: [ attributes add: #osx ].
	^ attributes asArray
]
