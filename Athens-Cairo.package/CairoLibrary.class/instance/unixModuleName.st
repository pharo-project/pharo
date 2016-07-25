accessing platform
unixModuleName
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit."
	
	#( 
		'/usr/lib/i386-linux-gnu/libcairo.so.2'
		'/usr/lib32/libcairo.so.2'
		'/usr/lib/libcairo.so.2') 
	do: [ :path | 
		path asFileReference exists ifTrue: [ ^ path ] ].
	
	self error: 'Cannot locate cairo library. Please check if it installed on your system'