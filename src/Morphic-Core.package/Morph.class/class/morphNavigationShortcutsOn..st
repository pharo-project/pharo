settings
morphNavigationShortcutsOn: aBuilder
	"Basic, general navigation shortcut among morphs. #MorphNoCtrl will not work for TextMorphs."

	<keymap>
	(aBuilder shortcut: #navigateFocusForwardCtrl)
		category: #MorphFocusCtrlNavigation
		default: Character tab ctrl asKeyCombination
		do: [ :target :morph :event | morph navigateFocusForward ].
	(aBuilder shortcut: #navigateFocusBackwardCtrl)
		category: #MorphFocusCtrlNavigation
		default: Character tab shift ctrl asKeyCombination
		do: [ :target :morph :event | morph navigateFocusBackward ].
	(aBuilder shortcut: #navigateFocusForward)
		category: #MorphFocusNavigation
		default: Character tab asKeyCombination
		do: [ :target :morph :event | morph navigateFocusForward ].
	(aBuilder shortcut: #navigateFocusBackward)
		category: #MorphFocusNavigation
		default: Character tab shift asKeyCombination
		do: [ :target :morph :event | morph navigateFocusBackward ]