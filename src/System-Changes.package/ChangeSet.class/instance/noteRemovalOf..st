class changes
noteRemovalOf: class
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteChangeType: #remove fromClass: class.
	changeRecords removeKey: class class name ifAbsent: [].