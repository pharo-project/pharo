private
hookFor: aLink
	| receiver hook arguments |
	
	(self hasOption: #optionDisabledLink for: aLink) ifTrue: [ ^RBSequenceNode new ].
	
	receiver := aLink metaObject isSymbol 
		ifTrue: [ self genReification: aLink metaObject for: aLink ]
		ifFalse: [  
			(self hasOption: #optionInlineMetaObject for: aLink)
				ifTrue: [RFLiteralVariableNode value: aLink metaObject]
				ifFalse: [RBMessageNode receiver: (RFLiteralVariableNode value: aLink)
											 selector: #metaObject ]].
	arguments := self generateArguments: aLink.
	
	hook := RFMessageNode 
		receiver: receiver
		selector: aLink selector 
		arguments: arguments.
	
	(self hasOption: #optionOneShot for: aLink) ifTrue: [
			hook := RBSequenceNode statements: {
				RBMessageNode
					receiver: (RFLiteralVariableNode value: aLink)
					selector: #uninstall.
				hook }].
	aLink hasCondition ifTrue: [hook := self wrapCondition: hook link: aLink].
	(aLink hasMetaLevel or: [self hasOption: #optionMetalevel for: aLink]) ifTrue: [hook := self wrapInContext: hook link: aLink].		
			
	^ hook