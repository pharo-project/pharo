Extension { #name : #SettingNodeBuilder }

{ #category : #'*Keymapping-Settings' }
SettingNodeBuilder >> category: aByteSymbol [ 
	node item categoryName: aByteSymbol
]

{ #category : #'*Keymapping-Settings' }
SettingNodeBuilder >> do: aBlockClosure [ 
	node item action: aBlockClosure 
]

{ #category : #'*Keymapping-Settings' }
SettingNodeBuilder >> shortcutName: aShortcutName [
	node item shortcutName: aShortcutName
]
