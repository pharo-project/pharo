spec accessing
action: aMessageSendOrABlock
	"set the action of the menu"
	
	aMessageSendOrABlock isBlock 
		ifTrue: [
			self target: aMessageSendOrABlock.
			self selector: #cull:.
			self arguments: {self model}]
		ifFalse: [
			self selector: aMessageSendOrABlock]
