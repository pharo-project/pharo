accessing platform
unixModuleName
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit.
	Packages such as snapcraft can also change the path.
	Look in a number of known locations, or LD_LIBRARY_PATH."

	(#('/usr/lib/i386-linux-gnu' '/usr/lib32' '/usr/lib'),
			(((OSEnvironment current getEnv: 'LD_LIBRARY_PATH') ifNil: [ '' ]) substrings: ':'))
		do: [ :path | 
			| libraryPath |
			libraryPath := path asFileReference / 'libcairo.so.2'.
			libraryPath exists
				ifTrue: [ ^ libraryPath fullName ] ].
	self error: 'Cannot locate cairo library. Please check if it installed on your system'