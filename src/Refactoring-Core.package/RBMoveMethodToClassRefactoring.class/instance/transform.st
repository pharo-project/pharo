transforming
transform
	| oldClass newClass rbMethod originalProtocol |
	oldClass := self classModelOf: method methodClass.
	newClass := self classModelOf: class.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocol.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	newClass compile: rbMethod source classified: {originalProtocol}