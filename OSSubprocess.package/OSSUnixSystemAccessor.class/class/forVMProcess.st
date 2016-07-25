instance creation
forVMProcess
	^ VMProcessSystemAcessor ifNil: [ VMProcessSystemAcessor := self basicNew initialize ]
