testing
testEncodedSlash
	self assert: 'http://example.com/foo//' asZnUrl pathPrintString = '/foo//'.
	self assert: 'http://example.com/foo//bar/' asZnUrl pathPrintString = '/foo//bar/'.
	self assert: 'http://example.com/foo//bar/file.txt' asZnUrl pathPrintString = '/foo//bar/file.txt'.

	self assert: 'http://example.com/foo/%2F' asZnUrl pathPrintString = '/foo/%2F'.
	self assert: 'http://example.com/foo/%2Fbar/' asZnUrl pathPrintString = '/foo/%2Fbar/'.
	self assert: 'http://example.com/foo/%2F/bar/' asZnUrl pathPrintString = '/foo/%2F/bar/'.
	self assert: 'http://example.com/foo/%2Ffoo.txt' asZnUrl pathPrintString = '/foo/%2Ffoo.txt'.

