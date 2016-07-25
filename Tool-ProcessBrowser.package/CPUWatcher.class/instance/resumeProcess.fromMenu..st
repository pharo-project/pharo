process operations
resumeProcess: aProcess fromMenu: aMenuMorph
	aMenuMorph delete.
	self processBrowser resumeProcess: aProcess.