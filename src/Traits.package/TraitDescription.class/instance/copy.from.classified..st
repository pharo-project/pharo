copying
copy: sel from: class classified: cat 
	"Install the method associated with the first arugment, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under the third 
	argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code := class sourceCodeAt: sel.
	code ifNotNil: 
			[cat
				ifNil: [category := class organization categoryOfElement: sel]
				ifNotNil: [category := cat].
			(self includesLocalSelector: sel)
				ifTrue: [code asString = (self sourceCodeAt: sel) asString 
							ifFalse: [self error: self name 
										, ' ' 
										, sel 
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]