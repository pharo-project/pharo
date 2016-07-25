event handling
keyStroke: event 
	"Process potential command keys that are not handled by keymappings yet"
	
	| args aCharacter |
	aCharacter := event keyValue asCharacter. 
	event anyModifierKeyPressed ifFalse: [
		self basicKeyPressed: aCharacter.
		^false].
	(self scrollByKeyboard: event) ifTrue: [^true].
	keystrokeActionSelector isNil ifTrue: [^false].
	(args := keystrokeActionSelector numArgs) = 1 
		ifTrue: [^model perform: keystrokeActionSelector with: aCharacter].
	args = 2 
		ifTrue: 
			[^model 
				perform: keystrokeActionSelector
				with: aCharacter
				with: self].
	^self 
		error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'