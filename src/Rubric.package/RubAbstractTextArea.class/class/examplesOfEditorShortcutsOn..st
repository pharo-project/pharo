shortcut examples
examplesOfEditorShortcutsOn: aBuilder
	"No Keymap here, by default, the editor rely on a builtin keymapping engine (see TextEditor and SmallEditor)
	Here are some example on how to declare them if you want to setup different shortcuts
	
	<keymap>

	(aBuilder shortcut: #accept)
		category: #RubTextEditor
		default: $s ctrl win | $s ctrl unix | $s command mac
		do: [ :morph | morph acceptContents ]. 
		
	(aBuilder shortcut: #selectAll) 
		category: #RubTextEditor
		shortcut: $a ctrl win | $a ctrl unix | $a command mac
		do: [ :target | target handleKeymapEdition: [:editor | editor selectAll ] ].

	(aBuilder shortcut: #copySelection) 
		category: #RubTextEditor
		default: $c ctrl win | $c ctrl unix | $c command mac
		do: [ :target | target editor copySelection ].
		
	(aBuilder shortcut: #paste) 
		category: #RubTextEditor
		default: $v ctrl win | $v ctrl unix | $v command mac
		do: [ :target | target handleKeymapEdition: [:editor | editor paste]].
		
	(aBuilder shortcut: #cut) 
		category: #RubTextEditor
		default: $x ctrl win | $x ctrl unix | $x command mac
		do: [ :target :event | target handleKeymapEdition: [:editor | editor cut]  ].
				
	(aBuilder shortcut: #find) 
		category: #RubTextEditor
		default: $f ctrl win | $f ctrl unix | $f command mac
		do: [ :target | target find ].
		
	(aBuilder shortcut: #findAgain) 
		category: #RubTextEditor
		default: $g ctrl win | $g ctrl unix | $g command mac
		do: [ :target | target handleKeymapEdition: [:editor | editor findAgain ] ].
		
	(aBuilder shortcut: #exchange) 
		category: #RubTextEditor
		default: $e ctrl win | $e ctrl unix | $e command mac
		do: [ :target | target handleKeymapEdition: [:editor | editor exchange ] ].
		
	(aBuilder shortcut: #backWord)  
		category: #RubTextEditor
		default: $w ctrl win | $w ctrl unix | $w command mac
		do: [ :target |  target handleKeymapEdition: [:editor | editor backWord ] ].
		
	(aBuilder shortcut: #backWord) 
		category: #RubTextEditor
		default: Character backspace  asShortcut win | Character backspace asShortcut unix | Character backspace asShortcut mac
		do: [ :target :morph :event |  target handleKeymapEdition: [:editor | editor backspace: event ] ].
		
	(aBuilder shortcut: #(swapChars)) 
		category: #RubTextEditor
		default: $y ctrl win | $y ctrl unix | $y command mac
		do: [ :target |  target handleKeymapEdition: [:editor | editor swapChars ] ].

	(aBuilder shortcut: #setSearchString)
		category: #RubTextEditor
		default: $h ctrl win | $h ctrl unix | $h command mac
		do: [ :target | target editor setSearchString ].
		
	(aBuilder shortcut: #cancel) 
		category: #RubTextEditor
		default: $l ctrl win | $l ctrl unix | $l command mac
		do: [ :target | target editor cancel ].
		
	(aBuilder shortcut: #undo) 
		category: #RubTextEditor
		default: $z ctrl win | $z ctrl unix | $z command mac
		do: [ :target | target editor undo ].
		
	(aBuilder shortcut: #redo) 
		category: #RubTextEditor
		default: $j ctrl shift win | $j ctrl shift unix | $j command shift mac
		do: [ :target | target editor redo ].
	
	(aBuilder shortcut: #cursorHome)
		category: #RubTextEditor
		default: Character home ctrl win | Character home ctrl unix | Character home command mac
		do: [ :target :morph :event | target editor cursorHome: event ].
	
	(aBuilder shortcut: #cursorEnd)
		category: #RubTextEditor
		default: Character end ctrl win | Character end ctrl unix | Character end command mac
		do: [ :target :morph :event | target editor cursorEnd: event ].

	(aBuilder shortcut: #cursorHome)
		category: #RubTextEditor
		default: Character home asShortcut win | Character home asShortcut unix | Character home asShortcut mac
		do: [ :target :morph :event | target editor cursorHome: event ].
	
	(aBuilder shortcut: #cursorEnd)
		category: #RubTextEditor
		default: Character end asShortcut win | Character end asShortcut unix | Character end asShortcut mac
		do: [ :target :morph :event | target editor cursorEnd: event ].
	"