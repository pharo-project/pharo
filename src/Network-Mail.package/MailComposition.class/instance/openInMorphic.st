interface
openInMorphic
	"open an interface for sending a mail message with the given initial 
	text"

	| textMorph buttonsList sendButton attachmentButton |
	morphicWindow := SystemWindow labelled: 'Mister Postman'.
	morphicWindow model: self.
	textMorph := self textModel newScrolledText
		beForPlainText;
		beWrapped;
		yourself.
	textMorph menuProvider: self selector:  #menuGet:shifted:.
	morphicWindow addMorph: textMorph frame: (0 @ 0.1 corner: 1 @ 1).
	buttonsList := AlignmentMorph newRow.
	sendButton := PluggableButtonMorph on: self getState: nil action: #submit.
	sendButton
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: 'send message';
		setBalloonText: 'Accept any unaccepted edits and add this to the queue of messages to be sent';
		onColor: Color white offColor: Color white.
	buttonsList addMorphBack: sendButton.
	attachmentButton := PluggableButtonMorph on: self getState: nil action: #addAttachment.
	attachmentButton
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: 'add attachment';
		setBalloonText: 'Send a file with the message';
		onColor: Color white offColor: Color white.
	buttonsList addMorphBack: attachmentButton.
	morphicWindow addMorph: buttonsList frame: (0 @ 0 extent: 1 @ 0.1).
	morphicWindow openInWorld