accessing
selectedClass: aClass
	"When the class seletion is changed, I send an event to UI"
	selectedClass := aClass.
	self triggerEvent: #updateSelectedClass
	