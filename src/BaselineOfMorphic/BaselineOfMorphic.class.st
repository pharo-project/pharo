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

	spec for: #common do: [
		spec package: 'System-Identification'.
		spec package: 'System-Settings-Core' with: [ spec requires: #( 'System-Identification' ) ].
		spec package: 'StartupPreferences'. "This should move out of Morphic but currently there is a cyclic dependency with System-Settings-Core..."

		spec package: 'System-Localization'.

		spec package: 'Graphics-Shapes'.
		spec package: 'Growl'.

		spec package: 'Native-Browser'.

		spec baseline: 'Regex' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		spec package: 'Morphic-Base'.
		spec package: 'Morphic-Widgets-Basic'.
		spec package: 'Morphic-Widgets-ColorPicker'.
		spec package: 'Morphic-Widgets-Extra'.
		spec package: 'Morphic-Widgets-Pluggable'.
		spec package: 'Morphic-Widgets-PolyTabs'.
		spec package: 'Morphic-Widgets-Scrolling'.
		spec package: 'Morphic-Widgets-Taskbar'.
		spec package: 'Morphic-Widgets-MissionControl'.

		spec package: 'Morphic-Widgets-Menubar'.
		spec package: 'Morphic-Widgets-Tree'.
		spec package: 'Morphic-Widgets-Windows'.
		spec package: 'Morphic-Widgets-FastTable' with: [ spec requires: #( 'Regex' ) ].
		"Halos plug into the world's root event handler, and open menus"
		spec package: 'Morphic-Halos'.
		"The world menu, the windows menu, the selection rectangle and the quit dialog"
		spec package: 'Morphic-Desktop' with: [ spec requires: #( 'Morphic-Halos' ) ].

		spec package: 'Morphic-Widgets-Taskbar-Tests'.
		spec package: 'Morphic-Widgets-MissionControl-Tests'.

		"Several packages depend on Rubric as a basic Morphic component"
		spec package: 'Rubric' with: [ spec requires: #( 'Regex' ) ].

		spec baseline: 'NewValueHolder' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'DarkBlueTheme'.

		spec package: 'Settings-Graphics'.
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
		spec package: 'Tool-Base'.

		"Sets up Morphic and starts its UI; the bootstrap runs it after loading Pharo"
		spec
			package: 'Morphic-Initialization'
			with: [
			spec requires: #( 'Morphic-Base' 'Morphic-Widgets-Menubar' 'Morphic-Widgets-Taskbar' 'Settings-Polymorph' 'Text-Diff' 'Morphic-Halos' 'Morphic-Desktop' ) ] ]
]

{ #category : 'baseline' }
BaselineOfMorphic >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]
