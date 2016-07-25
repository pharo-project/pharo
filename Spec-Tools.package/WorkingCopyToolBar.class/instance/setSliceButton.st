initialization
setSliceButton
	sliceButton := self instantiate: ButtonModel.
	sliceButton
		state: false;
		label: '+Slice';
		help: 'Add a slice to cleaning and nicely propose a bug fix';
		action: [ self createSlice ]