attributes
getSystemAttribute: attributeID 
	"Optional. Answer the string for the system attribute with the given 
	integer ID. Answer nil if the given attribute is not defined on this 
	platform. On platforms that support invoking programs from command 
	lines (e.g., Unix), this mechanism can be used to pass command line 
	arguments to programs written in Pharo.

	By convention, the first command line argument that is not a VM
	configuration option is considered a 'document' to be filed in. Such a
	document can add methods and classes, can contain a serialized object,
	can include code to be executed, or any combination of these.

	Currently defined attributes include: 
	-1000   1000th command line argument that specify VM options
   	...
	-1              first command line argument that specify VM options
      0               the full path name for currently executing VM
                       (or, on some platforms, just the path name of the VM's directory)
       1               full path name of this image (better use primImageName instead)
       2               a Squeak document to open, if any
       3               first command line argument for Squeak programs
       ...
       1000    1000th command line argument for Squeak programs
       1001    this platform's operating system 'Mac OS', 'Win32', 'unix', ...
       1002    operating system version
       1003    this platform's processor type
       1004    vm version
       1005    window system name
       1006    vm build id
       1007    Interpreter class (Cog VM only)
       1008    Cogit class (Cog VM only)
       1201    max filename length (Mac OS only)
       1202    file last error (Mac OS only)
       10001   hardware details (Win32 only)
       10002   operating system details (Win32 only)
       10003   graphics hardware details (Win32 only)"

	<primitive: 149>
	^ nil