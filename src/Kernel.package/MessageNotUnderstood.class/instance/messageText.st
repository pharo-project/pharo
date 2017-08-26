accessing
messageText
	"Return an exception's message text."
	
	^ messageText 
			ifNil:
				[message 
					ifNil: [super messageText]
					ifNotNil: [
						message lookupClass == UndefinedObject
							ifTrue: ['receiver of "{1}" is nil' format: {message selector asString}]
							ifFalse: [message lookupClass printString, '>>', message selector asString]]]