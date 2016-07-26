accessing
shortcutAsString 
	"Answers a string explaining shortcut"
	self shortcut ifNil: [ ^ '' ].
	^ String streamContents: [ :stream |
		stream << Smalltalk os menuShortcutModifierString << '+'.
		self shortcut isUppercase 
			ifTrue: [ stream << 'shift+' ].
		stream << self shortcut asLowercase asString ]