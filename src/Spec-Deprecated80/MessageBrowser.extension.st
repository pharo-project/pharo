Extension { #name : #MessageBrowser }

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> browseClass [
	self deprecated: 'This method seems never called.'.
	self currentMethod ifNotNil: [ :method | method methodClass browse ]
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> currentMethod [
	self deprecated: 'Use #selectedMessage instead' transformWith: '`@receiver currentMethod' -> '`@receiver selectedMessage'.
	^ self selectedMessage
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> listModel [
	self deprecated: 'It is not wise to allow external user access the content of a sub presenter. If this is accessed directly, the behavior should probably be moved to the MessageBrowser.'.
	^ messageList listModel
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> model [ 
	self deprecated: 'User should not use model directly.'.
	^ messageList model
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> resetSelection [
	self deprecated: 'I have the impression this method is not use. If it end up been call, please open an issue on pharo-spec/Spec'
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> selectedItem [
	self deprecated: 'Use #selectedMessage instead' transformWith: '`@receiver selectedItem' -> '`@receiver selectedMessage'.
	^ self selectedMessage
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> selectedMethods [
	self deprecated: 'I have the impression this method is not use. If it end up been call, please open an issue on pharo-spec/Spec'.
	^ {self selectedMessage} asOrderedCollection
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> setSelectedIndex: anIndex [
	self deprecated: 'Use #selectIndex: instead' transformWith: '`@receiver setSelectedIndex: `@argument' -> '`@receiver selectIndex: `@argument'.
	self selectIndex: anIndex
]

{ #category : #'*Spec-Deprecated80' }
MessageBrowser >> toolbarModel [
	self deprecated: 'Use #toolbarPresenter instead' transformWith: '`@receiver toolbarModel' -> '`@receiver toolbarPresenter'.
	^ self toolbarPresenter
]
