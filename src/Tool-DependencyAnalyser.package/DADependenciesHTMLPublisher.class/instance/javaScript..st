html utilities
javaScript: scriptFileName
	stream 
		<< '<script type="text/javascript" language="javascript" src="'
		<< scriptFileName 
		<< '"></script>'; lf