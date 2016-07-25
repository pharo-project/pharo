transforming
inlineMessagesInClass: aClass andSelector: aSelector 
	| messagesToInline previousCountOfMessages |
	previousCountOfMessages := 4294967295.	"Some really large number > # of initial self sends."
	
	[messagesToInline := self 
				numberOfSelfSendsIn: (aClass parseTreeFor: aSelector).
	messagesToInline > 0 and: [previousCountOfMessages > messagesToInline]] 
			whileTrue: 
				[| node |
				previousCountOfMessages := messagesToInline.
				node := self selfSendIn: (aClass parseTreeFor: aSelector).
				self onError: 
						[self performComponentRefactoring: (RBInlineMethodRefactoring 
									model: self model
									inline: node sourceInterval
									inMethod: aSelector
									forClass: aClass).
						numberReplaced := numberReplaced + 1]
					do: []].
	numberNotReplaced := numberNotReplaced + messagesToInline