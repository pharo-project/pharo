Extension { #name : #PragmaMenuAndShortcutRegistration }

{ #category : #'*Keymapping-Core' }
PragmaMenuAndShortcutRegistration >> attachShortcutCategory: aSymbol to: aClass [ 

	KMRepository default attachCategoryName: aSymbol to: aClass.
]

{ #category : #'*Keymapping-Core' }
PragmaMenuAndShortcutRegistration >> setAsGlobalCategory: aGlobalCategory [

	KMRepository default setAsGlobalCategory: aGlobalCategory
]
