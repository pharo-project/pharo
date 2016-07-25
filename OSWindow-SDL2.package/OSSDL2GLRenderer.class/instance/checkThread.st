misc
checkThread
	OSWindowRenderThread isThisThread ifFalse: [ ^ 'GL Renderer must be used inside the OSWindowRenderThread' ]