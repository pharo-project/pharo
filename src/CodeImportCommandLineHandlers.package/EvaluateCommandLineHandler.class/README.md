Usage: eval [--help] [--save] [ --no-quit ] <smalltalk expression>
	--help                   list this help message
	--save                   save the image after evaluation of the expression
	--no-quit                if specified, the image continues runing after
	                         evaluating the <smalltalk expression>
	<smallltalk expression>  a valid Smalltalk expression which is evaluated and 
	                         the result is printed on stdout

Documentation:
A CommandLineHandler that reads a string from the command line, outputs the evaluated result and quits the image. 

This handler either evaluates the arguments passed to the image:
	pharo Pharo.image eval  1 + 2
	
or it can read directly from stdin:

	echo "1+2" | $PHARO_VM my.image eval

Important: don't manually save the image at the end of the expression by calling something like 'Smalltalk snapshot: true andSave: true'! Instead, use the safer --save option.