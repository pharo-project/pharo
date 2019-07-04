Extension { #name : #SpDialogWindowPresenter }

{ #category : #'*Spec2-Deprecated' }
SpDialogWindowPresenter >> toolbar [
	[self deprecated: 'The way dialogs uses toolbars changed in Pharo 8 and this way is deprecated. 
	
	Before the dialog bar had one or two buttons: Ok and Cancel, but they where not configurable. Now the user can add itself multiple buttons and configure their action more easily.
	
	So here is how the new way works:
	
	If you do not override the method #initializeDialogWindow: in your dialog, two default buttons will be added: "Ok" and "Cancel". You can then configure their actions with the #okAction: and #cancelAction: selectors.
	
	If you do not want those default buttons you can override #initializeDialogWindow: and use this new API:
	
	aWindow 
		addButton: ''Debug''
		do: [ :presenter | self accept. presenter close ];
		addButton: ''Ignore'' 
		do: [ :presenter | presenter close ]. 

	'.].
	^ SpDeprecatedSpecToolbarHelper for: self
]
