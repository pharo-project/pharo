keyboard
handleKeystrokeBefore: aKeyboardEvent editor: anEditor
	"I return a boolean. 
	true when I have handled the event and no futher processing is needed by the caller."

	| keyCharacter controlKeyPressed |
	self flag: #fixme.	"this method should be split up"
	self setEditor: anEditor.
	self setModel: editor model.
	keyCharacter := aKeyboardEvent keyCharacter.
	controlKeyPressed := aKeyboardEvent controlKeyPressed.
	self isMenuOpen
		ifFalse: [ ^ self handleKeystrokeWithoutMenu: aKeyboardEvent ].
	(keyCharacter = Character home and: [ self captureNavigationKeys ])
		ifTrue: [ 
			menuMorph home.
			^ true ].
	(keyCharacter = Character end and: [ controlKeyPressed not and: [ self captureNavigationKeys ] ])
		ifTrue: [ 
			menuMorph end.
			^ true ].
	(keyCharacter = Character arrowRight and: [ self captureNavigationKeys ])
		ifTrue: [ 
			menuMorph showDetail.
			^ true ].
	(keyCharacter = Character arrowLeft and: [ self captureNavigationKeys ])
		ifTrue: [ ^ self leftArrow ].
	keyCharacter = Character arrowUp
		ifTrue: [ 
			menuMorph moveUp.
			^ true ].
	keyCharacter = Character arrowDown
		ifTrue: [ 
			menuMorph moveDown.
			^ true ].
	keyCharacter = Character pageUp
		ifTrue: [ 
			menuMorph pageUp.
			^ true ].
	keyCharacter = Character pageDown
		ifTrue: [ 
			menuMorph pageDown.
			^ true ].
	(keyCharacter = Character cr and: [ NECPreferences useEnterToAccept ])
		ifTrue: [ 
			menuMorph insertSelected
				ifTrue: [ ^ true ] ].
	keyCharacter = Character tab
		ifTrue: [ 
			NECPreferences expandPrefixes
				ifFalse: [ 
					menuMorph insertSelected
						ifTrue: [ ^ true ] ]
				ifTrue: [ 
					menuMorph insertCommonPrefixOrSelected
						ifTrue: [ ^ true ] ] ].
	self flag: #todo.	"Removing for now... most of the shortcuts are broken and no time or need now to fix them"	"(keyCharacter = $h and: [ controlKeyPressed ]) ifTrue: [ 
		menuMorph help.
		^ true ].

	(keyCharacter = $t and: [ controlKeyPressed ]) ifTrue: [ 
		menuMorph expand.
		^ true ].

	(keyCharacter = $u and: [ controlKeyPressed ]) ifTrue: [ 
		menuMorph switchToUntyped.
		^ true ].	"	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"
	keyCharacter = Character backspace
		ifTrue: [ 
			editor isCaretBehindChar
				ifFalse: [ self closeMenu ].
			^ false ].
	(controlKeyPressed not & aKeyboardEvent commandKeyPressed not
		and: [ aKeyboardEvent keyCharacter isCompletionCharacter ])
		ifFalse: [ 
			self closeMenu.
			^ keyCharacter = Character escape ].
	^ false