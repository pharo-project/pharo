Usage: test [--junit-xml-output] [--fail-on-failure] [<package> ...]
	--junit-xml-output    output the test results in a junit compatible format
	--fail-on-failure     if there is a test error or failure, it will exit with error code 1
	--fail-on-error       if there is a test error it will exit with error code 1
	--save                save after executing tests
	 <package>            a String matching a package name
	
Examples:
	#Run all the tests in the Tests-Exceptions package
	pharo Pharo.image test Tests-Exceptions
	
	#Run all the tests in packages matching Test-.* and KernelTests
	pharo Pharo.image test "Tests-.*" "KernelTests-.*"
	
	# Run test on a Hudson/Jenkins server
	pharo Pharo.image test --junit-xml-output "Tests-.*" "KernelTests-.*"
	