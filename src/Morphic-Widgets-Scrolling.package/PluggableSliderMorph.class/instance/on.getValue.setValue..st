instance creation
on: anObject getValue: getSel setValue: setSel
	"Use the given selectors as the interface."

	self
		model: anObject;
		getValueSelector: getSel;
		setValueSelector: setSel;
		updateValue