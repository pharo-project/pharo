interface
addAttachment
	| fileSelected |
	self textModel hasUnacceptedEdits
		ifTrue: [ self textModel acceptEditsInView ].
	fileSelected := UIManager default chooseFullFileNameMatching: nil.
	fileSelected ifNil: [ ^ self ].
	fileSelected
		writeStreamDo: [ :stream | 
			stream binary.
			self
				messageText:
					((MailMessage from: self messageText asString)
						addAttachmentFrom: stream withName: fileSelected basename;
						text;
						yourself) ]