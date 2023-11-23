Extension { #name : 'PragmaMenuAndShortcutRegistration' }

{ #category : '*Keymapping-Pragmas' }
PragmaMenuAndShortcutRegistration >> attachShortcutCategory: aSymbol to: aClass [

	KMRepository default attachCategoryName: aSymbol to: aClass
]

{ #category : '*Keymapping-Pragmas' }
PragmaMenuAndShortcutRegistration >> setAsGlobalCategory: aGlobalCategory [

	KMRepository default setAsGlobalCategory: aGlobalCategory
]
