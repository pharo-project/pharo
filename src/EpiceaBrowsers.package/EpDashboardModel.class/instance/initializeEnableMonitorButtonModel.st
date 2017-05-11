initialization
"protocol: initialization"
initializeEnableMonitorButtonModel

	enableMonitorButtonModel
		help: 'Enable/disable the Epicea Monitor. When enabled, it logs code changes (and some IDE events such as MC load/save and refactorings) performed in this Pharo session.';
		action: [ 
			self monitor enabled: enableMonitorButtonModel state not.
		].
