*Tool-Base
browseMessageList: messageList name: label 
	"Create and schedule a MessageSet browser on messageList."

	^ self   
		browseMessageList: messageList 
		name: label 
		autoSelect: nil