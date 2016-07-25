accessing
children
	^ self content dependencies flatCollect: 
		[ :dependency | 
		dependency users collect: 
			[ :association | | method sentMessages |
			method := association key.
			sentMessages := association value.
			DAMessageSendDependencyNode new
				content: method;
				parentNode: self;
				sentMessages: sentMessages 
				yourself ] ]