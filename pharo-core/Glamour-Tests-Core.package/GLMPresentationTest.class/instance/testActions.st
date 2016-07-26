tests
testActions
	| listing |
	listing := GLMPresentation new.
	listing actions isEmpty.
	listing addAction: GLMAction new.
	listing actions size = 1.