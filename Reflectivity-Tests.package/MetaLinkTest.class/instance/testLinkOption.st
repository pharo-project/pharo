tests - options
testLinkOption
	| link |
	link := MetaLink new 
		options: #(+option1 -option2).
	
	self assert: link option1.
	self deny: link option2.