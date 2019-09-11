Extension { #name : #MessageBrowser }

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> browseClass [
	self deprecated: 'This method seems never called.'.
	self currentMethod ifNotNil: [ :method | method methodClass browse ]
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> currentMethod [
	self deprecated: 'Use #selectedMessage instead' transformWith: '`@receiver currentMethod' -> '`@receiver selectedMessage'.
	^ self selectedMessage
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> listModel [
	self deprecated: 'It is not wise to allow external user access the content of a sub presenter. If this is accessed directly, the behavior should probably be moved to the MessageBrowser.'.
	^ messageList listModel
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> model [ 
	self deprecated: 'User should not use model directly.'.
	^ messageList model
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> resetSelection [
	self deprecated: 'I have the impression this method is not use. If it end up been call, please open an issue on pharo-spec/Spec'
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> selectedItem [
	self deprecated: 'Use #selectedMessage instead' transformWith: '`@receiver selectedItem' -> '`@receiver selectedMessage'.
	^ self selectedMessage
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> selectedMethods [
	self deprecated: 'I have the impression this method is not use. If it end up been call, please open an issue on pharo-spec/Spec'.
	^ {self selectedMessage} asOrderedCollection
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> setSelectedIndex: anIndex [
	self deprecated: 'Use #selectIndex: instead' transformWith: '`@receiver setSelectedIndex: `@argument' -> '`@receiver selectIndex: `@argument'.
	self selectIndex: anIndex
]

{ #category : #'*Spec2-Deprecated-Tools' }
MessageBrowser >> toolbarModel [
	self deprecated: 'Use #toolbarPresenter instead' transformWith: '`@receiver toolbarModel' -> '`@receiver toolbarPresenter'.
	^ self toolbarPresenter
]
