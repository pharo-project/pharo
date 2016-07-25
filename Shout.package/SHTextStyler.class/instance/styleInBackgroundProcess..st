styling
styleInBackgroundProcess: aText
	self terminateBackgroundStylingProcess.
	stylingEnabled ifFalse: [ ^ self ].
	text := aText copy.
	self monitor critical: [
		backgroundProcess := [ 
			self privateStyle: text.
			view ifNotNil: [
				view stylerStyledInBackground: text ] ] forkAt: Processor userBackgroundPriority ]