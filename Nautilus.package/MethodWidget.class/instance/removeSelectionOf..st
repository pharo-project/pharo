selection
removeSelectionOf: element
	"I do not remove immediatly the element because if the method have been compile, this will not be the same object anymore. So I check the class and the selector of the method."

	methodsSelection detect: [ :meth | meth methodClass = element methodClass and: [ meth selector = element selector ] ] ifFound: [ :meth | methodsSelection remove: meth ] ifNone: [ ^ self ]