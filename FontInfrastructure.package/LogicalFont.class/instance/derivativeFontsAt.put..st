derivatives
derivativeFontsAt: index put: aFont

	derivatives ifNil:[derivatives := Array new: 32].
	derivatives at: index put: aFont