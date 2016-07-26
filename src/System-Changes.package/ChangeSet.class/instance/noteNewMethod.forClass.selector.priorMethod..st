change logging
noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteNewMethod: newMethod selector: selector priorMethod: methodOrNil
