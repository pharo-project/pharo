transforming
transform
	| oldClass newClass rbMethod |
	oldClass := RBClassModelFactory rbClass named: method methodClass name.
	oldClass model: self model.
	newClass := RBClassModelFactory rbClass named: class name.
	newClass model: self model.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	newClass compile: rbMethod source