Extension { #name : #CmCommand }

{ #category : #'*Spec2-Commander2' }
CmCommand >> asSpecCommand [
	"Subclasses might override this method to define default icon and shortcut."
	^ self decorateWith: SpCommand
]

{ #category : #'*Spec2-Commander2' }
CmCommand >> asSpecCommandWithIconNamed: aSymbol [
	^ self asSpecCommand
		iconName: aSymbol;
		yourself
]

{ #category : #'*Spec2-Commander2' }
CmCommand >> asSpecCommandWithIconNamed: aSymbol shortcutKey: aKMKeyCombination [
	^ self asSpecCommand
		iconName: aSymbol;
		shortcutKey: aKMKeyCombination;
		yourself
]

{ #category : #'*Spec2-Commander2' }
CmCommand >> asSpecCommandWithShortcutKey: aKMKeyCombination [
	^ self asSpecCommand
		shortcutKey: aKMKeyCombination;
		yourself
]

{ #category : #'*Spec2-Commander2' }
CmCommand class >> forSpec [
	^ self new
		asSpecCommand
]

{ #category : #'*Spec2-Commander2' }
CmCommand class >> forSpecContext: anObject [

	^ self forSpec 
	context: anObject;
	yourself
]

{ #category : #'*Spec2-Commander2' }
CmCommand class >> forSpecWithIconNamed: aSymbol [
	^ self new asSpecCommandWithIconNamed: aSymbol
]

{ #category : #'*Spec2-Commander2' }
CmCommand class >> forSpecWithIconNamed: aSymbol shortcutKey: aKMKeyCombination [
	^ self new
		asSpecCommandWithIconNamed: aSymbol shortcutKey: aKMKeyCombination
]
