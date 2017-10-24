baseline
baseline: spec 
    
	<baseline>
        
	| repository | 
	
	repository := 'filetree://./pharo-core/src'.    
	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'MorphicCore' with: [spec repository: repository].

		spec package: 'System-Localization'.
		spec package: 'Collections-DoubleLinkedList'.
		spec package: 'Text-Core'.
		spec package: 'System-Clipboard'.
		
		spec package: 'Balloon'.
		spec package: 'EmbeddedFreeType'.
		spec package: 'EmergencyEvaluator'.
		spec package: 'FileSystem-Zip'.
		spec package: 'FontInfrastructure'.
		spec package: 'FreeType'.
		spec package: 'Fuel'.
		spec package: 'FuelPlatform-Core'.
		spec package: 'FuelPlatform-Pharo-Core'.
		spec package: 'FuelPlatform-Pharo-06'.
	
		spec package: 'FuelCommandLineHandler'.
		spec package: 'FuelSystem-FileRegistry'.

		spec package: 'Fonts-Abstract'.

		spec package: 'Graphics-Files'.
		spec package: 'Graphics-Fonts'.
		spec package: 'Graphics-Shapes'.
		spec package: 'Growl'.

		spec package: 'MenuRegistration'.

		spec package: 'Keymapping-Core'.
		spec package: 'Keymapping-KeyCombinations'.
		spec package: 'Keymapping-Pragmas'.

		spec package: 'Morphic-Base'.
		spec package: 'Morphic-Examples'.
		spec package: 'Morphic-Widgets-Basic'.
		spec package: 'Morphic-Widgets-ColorPicker'.
		spec package: 'Morphic-Widgets-Extra'.
		spec package: 'Morphic-Widgets-List'.
		spec package: 'Morphic-Widgets-Pluggable'.
		spec package: 'Morphic-Widgets-PolyTabs'.
		spec package: 'Morphic-Widgets-Scrolling'.
		spec package: 'Morphic-Widgets-Tabs'.
		spec package: 'Morphic-Widgets-Taskbar'.
		spec package: 'Morphic-Widgets-Tree'.
		spec package: 'Morphic-Widgets-Windows'.
		spec package: 'Multilingual-OtherLanguages'.
		spec package: 'Multilingual-TextConverterOtherLanguages'.

		spec package: 'NewValueHolder'.

		spec package: 'Polymorph-Widgets'.

		spec package: 'Settings-Graphics'.
		spec package: 'Settings-Polymorph'.
		spec package: 'Settings-System'.

		spec package: 'System-Caching'.
		spec package: 'System-History'.
		spec package: 'System-Installers'.
		spec package: 'System-Settings'.
		spec package: 'System-Sound'.
		
		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.

		spec package: 'Unicode-Initialization'.

		spec package: 'Morphic-Widgets-TickList'.
		
		spec package: 'System-Changes-FileServices'.
		spec package: 'System-OSEnvironments'.

		spec package: 'StartupPreferences'.
		spec package: 'System-Identification'.
	].
