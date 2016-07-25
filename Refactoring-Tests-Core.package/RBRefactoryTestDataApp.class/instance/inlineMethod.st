test
inlineMethod
	| temp |
	temp := self
				foo;
				inlineMethod;
				bar.
	^temp