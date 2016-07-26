file paths
unixFontDirectories

	"Answer the unix/linux font folder paths"
	^ #('/usr/share/fonts' '/usr/local/share/fonts') 
		collect: [ :each| each asFileReference ]
		thenSelect: [ :each| each exists ].