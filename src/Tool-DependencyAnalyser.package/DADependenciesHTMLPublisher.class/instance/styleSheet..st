html utilities
styleSheet: styleSheetUrl
	stream 
		<< '<link rel="stylesheet" type="text/css" href="'
		<< styleSheetUrl 
		<< '">'; lf