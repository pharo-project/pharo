menu - change set
browseChangeSet
	"Open a message list browser on methods in the current change set"

	| messages |
	messages := self selectedChangeSet changedMessageList select: [ :each | each isValid].	
	Smalltalk tools messageList
		browse: messages 
		title: 'Methods in Change Set ', self selectedChangeSet name .