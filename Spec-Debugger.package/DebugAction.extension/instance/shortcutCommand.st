*spec-debugger
shortcutCommand
	self flag: 'hacky solution to still be compatible with the GTDebugger (for the moment)'.
	^ self keyText ifNotNil: [:aString |
		aString first isUppercase
			ifTrue: [ aString first meta shift ]
			ifFalse: [ aString first meta ]  ]