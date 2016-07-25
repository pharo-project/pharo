instructions
pushConsArray: size
	stack push.
	stack pop: size.
	encoder genPushConsArray: size