transforming
transform
	| oldClass newClass rbMethod |
	oldClass := RBClass named: method methodClass name.
	oldClass model: self model.
	newClass := RBClass named: class name.
	newClass model: self model.
	rbMethod := RBMethod for: newClass source: method sourceCode selector: method selector.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	newClass compile: rbMethod source