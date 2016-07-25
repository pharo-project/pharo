running
setUp
	cache := ASTCache default copy.
	counter := 0.
	link := MetaLink new
		metaObject: self;
		selector: #increment.
	node := (self class >> #annotatedMethod) ast statements last value.
	node link: link