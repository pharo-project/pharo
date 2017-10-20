initialization
setSliceButton
	sliceButton := self instantiate: ButtonPresenter.
	sliceButton
		state: false;
		label: '+Slice';
		help: 'Add a slice to cleaning and nicely propose a bug fix';
		action: [ self createSlice ]