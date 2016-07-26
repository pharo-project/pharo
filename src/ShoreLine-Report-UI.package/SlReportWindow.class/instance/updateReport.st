initialization
updateReport

	self report: (self reporter generateReportForDebugger: self debugger).
	self displayReportContents.