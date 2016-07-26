*GT-InspectorExtensions-Core
gtExampleKeymapFOO
	<gtExample>
	<label: 'Keymap: FOO'>
	
	^ self 
		named: #Foo
		shortcut: $F asKeyCombination, $O asKeyCombination, $O asKeyCombination
		action: [ ]