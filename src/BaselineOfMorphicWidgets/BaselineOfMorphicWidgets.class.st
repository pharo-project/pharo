"
I load Morphic's widgets on top of `BaselineOfMorphicCore`: windows, buttons, lists, trees, tables, menus, themes, and Rubric's text widgets and editors.

An application that opens windows needs me, but not the desktop. `BaselineOfMorphicDesktop` builds the desktop on top of me.
"
Class {
	#name : 'BaselineOfMorphicWidgets',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMorphicWidgets',
	#package : 'BaselineOfMorphicWidgets'
}

{ #category : 'baseline' }
BaselineOfMorphicWidgets >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [ "Plain morphs, events, layouts and worlds. The widgets build on them"
		spec baseline: 'MorphicCore' with: [ spec repository: repository ].
		"Menus are built from pragmas and menu specs"
		spec baseline: 'MenuRegistration' with: [ spec repository: repository ].
		"Rubric's debug it submits an Oups debug request"
		spec baseline: 'Debugging' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].
		"Tables sort with sort functions"
		spec baseline: 'SortFunctions' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].
		spec baseline: 'Regex' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		spec package: 'System-Localization'.
		spec package: 'Graphics-Shapes'.
		"Icon packs. IconPacks, in the IDE, downloads them"
		spec package: 'Graphics-Icons' with: [ spec requires: #( 'MorphicCore' ) ].
		"The clipboard, for copy and paste"
		spec package: 'System-Utilities'.
		"Jobs announce their progress to the progress bar"
		spec package: 'Jobs'.
		spec package: 'System-History'.
		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.
		"Finds the selector under the cursor of the code editor"
		spec package: 'Tools-CodeNavigation'.
		spec package: 'Tool-Base' with: [ spec requires: #( 'MorphicCore' 'Debugging' ) ].

		"Plain widgets, buttons, lists, tabs, and Rubric's text widgets and editors"
		spec package: 'Morphic-Widgets' with: [
			spec requires:
				#( 'MorphicCore' 'Graphics-Icons' 'Graphics-Shapes' 'Tools-CodeNavigation' 'System-History' 'System-Localization'
				   'Text-Diff' 'Text-Edition' 'Text-Scanning' 'Tool-Base' 'Debugging' 'Jobs' 'MenuRegistration' 'System-Utilities' ) ].
		spec package: 'Morphic-Widgets-Menubar' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-Tree' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-FastTable' with: [ spec requires: #( 'Morphic-Widgets' 'Regex' 'SortFunctions' ) ].
		spec package: 'DarkBlueTheme' with: [ spec requires: #( 'Morphic-Widgets' ) ].

		spec package: 'Graphics-Icons-Tests' with: [ spec requires: #( 'Graphics-Icons' ) ].
		spec package: 'Rubric-Tests' with: [ spec requires: #( 'Morphic-Widgets' ) ].
		spec package: 'Morphic-Widgets-FastTable-Tests' with: [ spec requires: #( 'Morphic-Widgets-FastTable' ) ].

		spec
			group: 'default'
			with: #( 'Morphic-Widgets' 'Morphic-Widgets-Menubar' 'Morphic-Widgets-Tree' 'Morphic-Widgets-FastTable'
				   'DarkBlueTheme' );
			group: 'Tests' with: #( 'default' 'Graphics-Icons-Tests' 'Rubric-Tests' 'Morphic-Widgets-FastTable-Tests' ) ]
]
