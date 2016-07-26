actions
executeWithDevWorkflow: aCommandClass
	browser projectPanel project
		ifNotNil: [ :project | | cmd |
			 cmd := aCommandClass 
						target: project	devWorkflow
						for: self.
			[ cmd executeWithDevWorkflow: project devWorkflow ]
				on: MetacelloValidationFailure 
				do: [ :error | (VSConfigurationValidationIssuesDisplayCommand target: error for: self) execute] ].
	browser refreshConfiguration.
	browser selectDevelopmentVersion.
	browser update