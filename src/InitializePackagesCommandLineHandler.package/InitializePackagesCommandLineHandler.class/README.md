Usage: initializePackages [ --protocols=<protocols> ] [ --packages=<packages> ]  [ --save ]
	protocols	A name of a file with method protocols specification
	packages	a name of a file with packages list
	
Documentation:
Initializes packages structure from information provided in text files. This is used to initialize 
packages structure for bootstrapped images. Do not use it on full Pharo images.

Protocols are provided in file where every method has one line and provides following data separated by  tabs:
- class name
- method is on the class side (true/false)
- method selector
- protocol name

The <cr> line ending is expected for protocols and packages file.

Protocols file exxample:
RBReplaceRule	false	searchForTree:replaceWith:	matching
RBPatternPragmaNode	true	selector:keywordsPositions:arguments:	instance creation
			
Packages file contains list of packages where every package is on own line.

Examples:
	pharo Pharo.image initializePackages --protocols=protocols.txt --packages=packages.txt --save