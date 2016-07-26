rendering browsers
renderWizard: aGLMWizard

	|tmpRenderer|
	tmpRenderer :=  GLMMorphicWizardRenderer new.
	aGLMWizard  setSpecificAnnouncementActionsTo: tmpRenderer.  
	^ (tmpRenderer renderer: self) render: aGLMWizard