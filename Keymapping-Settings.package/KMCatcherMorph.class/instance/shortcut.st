api
shortcut
	| theKeystrokes |
	theKeystrokes := self keystrokes.
	theKeystrokes 
		ifEmpty: [ ^ KMNoShortcut new ].
		
	^ theKeystrokes allButFirst 
		inject: theKeystrokes first asKeyCombination 
		into: [ :acum :each | 
			acum , each asKeyCombination ]