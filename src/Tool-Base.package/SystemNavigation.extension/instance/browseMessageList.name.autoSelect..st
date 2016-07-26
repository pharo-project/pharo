*Tool-Base
browseMessageList: messageList name: labelString autoSelect: autoSelectString
	"By default it never refreshes"
	^self browseMessageList: messageList name: labelString autoSelect: autoSelectString refreshingBlock: [ :method | false ]