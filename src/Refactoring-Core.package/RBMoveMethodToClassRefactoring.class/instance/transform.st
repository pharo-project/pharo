transforming
transform
	| oldClass newClass rbMethod originalProtocol |
	oldClass := RBClassModelFactory rbClass named: method methodClass name.
	oldClass model: self model.
	newClass := RBClassModelFactory rbClass named: class name.
	newClass model: self model.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocol.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	newClass compile: rbMethod source classified: {originalProtocol}