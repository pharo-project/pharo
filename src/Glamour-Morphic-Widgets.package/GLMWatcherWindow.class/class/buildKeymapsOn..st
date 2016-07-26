instance creation
buildKeymapsOn: aBuilder
	<keymap>
	
	(aBuilder shortcut: #openGlamourWatcher) 
		category: #Glamour
		default: Character space meta shift 
		do: [ :morph | self uniqueInstance toggleOpen ].

	aBuilder attachShortcutCategory: #Glamour to: Morph.