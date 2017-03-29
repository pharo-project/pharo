popup - layout
popupLayout
	"Return my current popup layout.
	popupLayout can not be nil"
	<return: #GLMPopupBrickLayout>

	self
		assert: [ popupLayout isNotNil ]
		description: [ 'Popup layout can not be nil' ].

	^ popupLayout