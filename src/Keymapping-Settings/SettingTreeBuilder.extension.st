Extension { #name : #SettingTreeBuilder }

{ #category : #'*Keymapping-Settings' }
SettingTreeBuilder >> attachShortcutCategory: aByteSymbol to: aClass [
	KMFactory keymapContainer attachCategoryName: aByteSymbol to: aClass.
]

{ #category : #'*Keymapping-Settings' }
SettingTreeBuilder >> shortcut: aShortcutNameSymbol [
	^ self nodeClass: KMShortcutDeclaration name: aShortcutNameSymbol
]
