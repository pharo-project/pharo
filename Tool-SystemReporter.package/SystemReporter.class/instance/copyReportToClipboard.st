accessing-report
copyReportToClipboard
	Clipboard clipboardText: self reportText.
	UIManager default inform: 'Copied Report to Clipboard'