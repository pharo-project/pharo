"
I load the Morphic desktop on top of `BaselineOfMorphicWidgets`: halos, the taskbar, mission control, notifications, the world menu, file drops and the settings, and `Morphic-Initialization`, which starts the desktop UI.

`BaselineOfMorphic` loads me with the widgets.
"
Class {
	#name : 'BaselineOfMorphicDesktop',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMorphicDesktop',
	#package : 'BaselineOfMorphicDesktop'
}

{ #category : 'baseline' }
BaselineOfMorphicDesktop >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		spec baseline: 'MorphicWidgets' with: [ spec repository: repository ].
		spec baseline: 'NewValueHolder' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'System-Identification'.
		spec package: 'System-Settings-Core' with: [ spec requires: #( 'System-Identification' 'MorphicWidgets' ) ].
		spec package: 'StartupPreferences'. "This should move out of Morphic but currently there is a cyclic dependency with System-Settings-Core..."
		spec package: 'PragmaCollector'.
		spec package: 'Native-Browser'.
		"File drops on the desktop"
		spec package: 'System-FileRegistry'.

		spec package: 'Growl' with: [ spec requires: #( 'MorphicWidgets' ) ].
		spec package: 'Morphic-Widgets-Taskbar' with: [ spec requires: #( 'MorphicWidgets' ) ].
		spec package: 'Morphic-Widgets-MissionControl' with: [ spec requires: #( 'MorphicWidgets' ) ].
		"Halos plug into the world's root event handler, and open menus"
		spec package: 'Morphic-Halos' with: [ spec requires: #( 'MorphicWidgets' ) ].
		"The world menu, the windows menu, the selection rectangle and the quit dialog"
		spec
			package: 'Morphic-Desktop'
			with: [ spec requires: #( 'Morphic-Halos' 'Morphic-Widgets-Taskbar' 'Growl' 'System-FileRegistry' ) ].

		spec package: 'Settings-Graphics' with: [ spec requires: #( 'MorphicWidgets' ) ].
		spec package: 'Settings-Polymorph' with: [ spec requires: #( 'Morphic-Halos' ) ].
		spec package: 'Settings-System'.
		"the following package is listed here to be after Setting core 
		however conceptually it could be placed somewhere else."
		spec package: 'Formatter-Settings'.

		"Sets up Morphic and starts its UI; the bootstrap runs it after loading Pharo"
		spec
			package: 'Morphic-Initialization'
			with: [ spec requires: #( 'MorphicWidgets' 'Morphic-Widgets-Taskbar' 'Settings-Polymorph' 'Morphic-Halos' 'Morphic-Desktop' ) ].

		spec package: 'Morphic-Widgets-Taskbar-Tests' with: [ spec requires: #( 'Morphic-Widgets-Taskbar' ) ].
		spec package: 'Morphic-Desktop-Tests' with: [ spec requires: #( 'Morphic-Desktop' ) ].
		spec package: 'Morphic-Widgets-MissionControl-Tests' with: [ spec requires: #( 'Morphic-Widgets-MissionControl' ) ].

		spec group: 'Tests' with: #( 'Morphic-Widgets-Taskbar-Tests' 'Morphic-Desktop-Tests' 'Morphic-Widgets-MissionControl-Tests' ) ]
]

{ #category : 'baseline' }
BaselineOfMorphicDesktop >> project [
	"Load atomically: System-Settings-Core and StartupPreferences depend on each other"

	| aProject |
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
