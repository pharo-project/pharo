change logging
removeSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info
	"Include indication that a method has been forgotten.
	info is a pair of the source code pointer and message category
	for the method that was removed."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: info
