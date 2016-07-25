The CommandLineArguments represents the arguments passed to the image.
In the following case,

	$PHARO_VM myImage.image --foo bar
	
`CommandLineArguments default` contains {'--foo'. 'bar'}.