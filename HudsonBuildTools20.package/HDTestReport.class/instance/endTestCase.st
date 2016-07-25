private
endTestCase
	stream tab; nextPutAll: '</testcase>'; lf.
	progressFile nextPutAll: 'finished' ; crlf; flush.
