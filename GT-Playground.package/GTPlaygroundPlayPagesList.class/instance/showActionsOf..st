actions
showActionsOf: aRowBrick
	aRowBrick subbricks select: [ :each | each isKindOf: GLMHintableActionButtonBrick ] thenDo: [ :each | each visible: true ]