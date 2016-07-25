accessing
selectedChangeWrapper: aWrapper
	"Set the selected change."

	selectedChangeWrapper := aWrapper.
	self
		changed: #selectedChangeWrapper;
		updateSource;
		changed: #compositeText