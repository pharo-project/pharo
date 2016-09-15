initialization
initializeWidgets

	self instantiateModels: {
		#logModel -> #EpLogBrowser.
		#enableMonitorButtonModel -> #ButtonModel.
		#activeLogTextModel -> #TextInputFieldModel.
		#createActiveLogButtonModel -> #ButtonModel.
		#logSetButtonModel -> #ButtonModel.
		#activeLogLabelModel -> #LabelModel.
	}