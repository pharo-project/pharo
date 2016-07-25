transforming
deprecateMethod: aSelector in: aClass using: newSel
	oldSelector := aSelector.
	newSelector := newSel.
	class := self classObjectFor: aClass