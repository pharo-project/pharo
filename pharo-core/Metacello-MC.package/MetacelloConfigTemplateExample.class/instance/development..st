tags
development: spec
	"By convention the development branch should point to a fixed version that is regularly updated and might contain unstable code.
	The name used by Metacello is only defined by the following pragma:"
	<symbolicVersion: #development >
	
	"For the development tag refer to a fixed version which you update if you commit new code.
	Note that you can refer here to any other version name from this configuration"
	spec for: #'common' version: 'dev'.