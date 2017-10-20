Usage: st [--help] [ --quit ] <FILE>
	--help    list this help message
	--quit    if specified, the image exits without saving after evaluating FILE
	--save    if specified, save the image after evaluating FILE
	--no-source if specified, do not write to the .changes file
	<FILE>    a file containing valid Pharo expressions

Documentation:
The ST command line handler runs Pharo code stored in a file without quiting.

Example:

	pharo Pharo.image st code.st
	
	# By default files ending in .st are recognized and evaluated
	pharo Pharo.image code.st
