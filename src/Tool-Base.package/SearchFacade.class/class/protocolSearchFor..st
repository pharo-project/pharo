protocol search
protocolSearchFor: aClass

	^ (self protocolSearchIn: aClass organization categories)
			browseBlock: [:answer | Smalltalk tools browser openOnClass: aClass onProtocol: answer ]; 
			yourself