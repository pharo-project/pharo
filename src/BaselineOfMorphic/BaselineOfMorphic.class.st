"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfMorphicCore

Load type: 
- atomic

Installs:
- Fuel
- Text and fonts support
- Morphic widgets
- Polymorph
- settings
- some other packages
"
Class {
	#name : 'BaselineOfMorphic',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMorphic',
	#package : 'BaselineOfMorphic'
}

{ #category : 'baseline' }
BaselineOfMorphic >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [ "Plain morphs, events, layouts and worlds. The widgets build on them"
		spec baseline: 'MorphicCore' with: [ spec repository: repository ].

		spec package: 'System-Identification'.
		spec package: 'System-Settings-Core' with: [ spec requires: #( 'System-Identification' 'Graphics-Icons' ) ].
		spec package: 'StartupPreferences'. "This should move out of Morphic but currently there is a cyclic dependency with System-Settings-Core..."

		spec package: 'System-Localization'.

		spec package: 'Graphics-Shapes'.
		"Icon packs: they download their icons over HTTP"
		spec package: 'Graphics-Icons' with: [ spec requires: #( 'MorphicCore' ) ].
		spec package: 'Growl' with: [ spec requires: #( 'Morphic-Widgets' ) ].

		spec package: 'Native-Browser'.

		spec baseline: 'Regex' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		"Finds the selector under the cursor of the code editor"
		spec package: 'Tools-CodeNavigation'.
		"Plain widgets, buttons, lists, tabs, and Rubric's text widgets and editors"
		spec package: 'Morphic-Widgets' with: [
			spec requires:
				#( 'MorphicCore' 'Graphics-Icons' 'Graphics-Shapes' 'Regex' 'Tools-CodeNavigation' 'System-History' 'Text-Diff' 'Text-Edition'
				   'Text-Scanning' 'Tool-Base' ) ].
		spec package: 'Morphic-Widgets-Taskbar' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-MissionControl' with: [ spec requires: #( 'Morphic-Widgets' ) ].

		spec package: 'Morphic-Widgets-Menubar' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-Tree' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-FastTable' with: [ spec requires: #( 'Morphic-Widgets' 'Regex' ) ].
		"Halos plug into the world's root event handler, and open menus"
		spec package: 'Morphic-Halos' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		"The world menu, the windows menu, the selection rectangle and the quit dialog"
		spec package: 'Morphic-Desktop' with: [ spec requires: #( 'Morphic-Halos' 'Morphic-Widgets-Taskbar' 'Growl' ) ].

		spec package: 'Morphic-Widgets-Taskbar-Tests' with: [ spec requires: #( 'Morphic-Widgets-Taskbar' ) ].
		spec package: 'Morphic-Desktop-Tests' with: [ spec requires: #( 'Morphic-Desktop' ) ].
		spec package: 'Morphic-Widgets-MissionControl-Tests' with: [ spec requires: #( 'Morphic-Widgets-MissionControl' ) ].

		spec baseline: 'NewValueHolder' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'DarkBlueTheme' with: [ spec requires: #( 'Morphic-Widgets' ) ].

		spec package: 'Settings-Graphics' with: [ spec requires: #( 'MorphicCore' ) ].
		spec package: 'Settings-Polymorph'.
		spec package: 'Settings-System'.

		spec package: 'System-History'.
		spec package: 'PragmaCollector'.
		"the following package is listed here to be after Setting core 
		however conceptually it could be placed somewhere else."
		spec package: 'Formatter-Settings'.

		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.

		"Tool-Base extends and uses morphic, so it depends on it"
		spec package: 'Tool-Base' with: [ spec requires: #( 'MorphicCore' ) ].

		"Sets up Morphic and starts its UI; the bootstrap runs it after loading Pharo"
		spec package: 'Morphic-Initialization' with: [
			spec requires:
				#( 'Morphic-Widgets' 'Morphic-Widgets-Menubar' 'Morphic-Widgets-Taskbar' 'Settings-Polymorph' 'Text-Diff' 'Morphic-Halos'
				   'Morphic-Desktop' ) ].

		spec group: 'Tests' with: #( 'Morphic-Widgets-Taskbar-Tests' 'Morphic-Desktop-Tests' 'Morphic-Widgets-MissionControl-Tests' ) ]
]

{ #category : 'baseline' }
BaselineOfMorphic >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]
