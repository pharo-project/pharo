html
comment: string
	"Write an HTML/XML comment consisting of string, as in <!-- string -->"
	
	stream
		nextPutAll: '<!-- ';
		nextPutAll: string;
		nextPutAll: ' -->'