testing
testDownloadSmallHTML
	| client |
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last.
	"First download to a directory"
	(client := ZnClient new)
		url: self smallHtmlUrl;
		downloadTo: ZnFileSystemUtils defaultDirectoryPath.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	ZnFileSystemUtils 
		oldFileNamed: self smallHtmlUrl pathSegments last
		do: [ :stream | self assert: (stream upToEnd includesSubstring: 'Small') ].
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last.
	"Second download to an explicitly named file"
	client 
		url: self smallHtmlUrl;
		downloadTo: self smallHtmlUrl pathSegments last.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	ZnFileSystemUtils 
		oldFileNamed: self smallHtmlUrl pathSegments last
		do: [ :stream | self assert: (stream upToEnd includesSubstring: 'Small') ].	
	client close.
	ZnFileSystemUtils deleteIfExists: self smallHtmlUrl pathSegments last
