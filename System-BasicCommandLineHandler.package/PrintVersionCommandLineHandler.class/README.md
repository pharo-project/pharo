Usage: printVersion [ --numeric | --release ]
	--numeric   Print the full version number only (e.g. 12345)
	--release   Print the major relase number only (e.g. 1.2)
	
Documentation:
Prints the version number in an easy to parse format. This can be used in Jenkins with the "Description Setter" Plugin. Configure it like this:

Regular expression:  \[version\] (.*)
Description: \1


Examples:
	pharo Pharo.image printVersion
	#result will be something like:
	[version] 3.0 #30100

	pharo Pharo.image printVersion --numeric
	# will print a simpler version
	30100
	
	pharo Pharo.image printVersion --release
	# prints the Pharo release version
	3.0