widget API
shortcutText
	| shortcut platform string |
	
	shortcut := self shortcut.
	shortcut isString ifTrue: [ shortcut := self defaultModifierShortcut ].
	shortcut ifNil: [ ^ nil ].
	platform := Smalltalk os platformFamily.
	shortcut combinationsDo: [ :each | (each platform = Smalltalk os platformFamily) ifTrue: [ shortcut := each ] ].
	
	string := '⌘⇧'.
	
	^ (platform =  #MacOSX and: [ 
		(StandardFonts menuFont hasGlyphsForAll: string) and: [
		string allSatisfy: [ :c | (StandardFonts menuFont characterFormAt: c) width~= 0 ]]])
		ifFalse: [ shortcut shortcut printString ]
		ifTrue: [ String streamContents: [:stream | shortcut shortcut prettyPrintOn: stream ] ]