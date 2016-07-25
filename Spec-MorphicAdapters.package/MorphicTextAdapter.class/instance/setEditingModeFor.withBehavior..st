private
setEditingModeFor: textArea withBehavior: behavior
	behavior
		ifNil:
			[
			self isForSmalltalkCode
				ifTrue: [ textArea beForSmalltalkScripting ]
				ifFalse: [ textArea beForPlainText ] ]
		ifNotNil: [ :class | 
			textArea
				beForSmalltalkCodeInClass: class ]