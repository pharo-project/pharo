Extension { #name : #ToolShortcutsCategory }

{ #category : #'*Iceberg-TipUI' }
ToolShortcutsCategory >> openIceberg [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openIcebergShortcut action: [ IceTipRepositoriesBrowser new openWithSpec ]
]
