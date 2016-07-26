*Tool-Base
browseMessageList: messageList name: labelString autoSelect: autoSelectString refreshingBlock: aBlock
	"Create and schedule a MessageSet browser on the message list."
	| methods |
	
	"Do not show trait methods"
	methods := messageList reject: [ :each| each isFromTrait ].
	methods isEmpty ifTrue: 
		[^ self inform: 'There are no ', String cr, labelString].
	
	^ Smalltalk tools messageList new
		messages: methods; 
		title: labelString; 
		autoSelect: autoSelectString;
		refreshingBlock: aBlock;
		open