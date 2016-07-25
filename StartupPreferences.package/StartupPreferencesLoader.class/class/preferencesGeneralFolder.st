private
preferencesGeneralFolder

	^ [ FileLocator preferences asFileReference / 'pharo' ]
		on: Error
		do: [ :ex |
			"Non NB-VMs will fail with a generic error"		
			StartupPreferencesLoadingError signal ]