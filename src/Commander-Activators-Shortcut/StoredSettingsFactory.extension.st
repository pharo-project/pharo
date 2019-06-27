Extension { #name : #StoredSettingsFactory }

{ #category : #'*Commander-Activators-Shortcut' }
StoredSettingsFactory >> visitCmdShortcutSetting: aCmdShortcutSetting [
	self newStoredSetting realValue: aCmdShortcutSetting shortcutActivation
]
