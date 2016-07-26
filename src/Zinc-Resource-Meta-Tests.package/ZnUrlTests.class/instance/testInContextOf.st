testing
testInContextOf
	| baseUrl url |
	baseUrl := 'http://zn.stfx.eu/foo.txt' asZnUrl.
	url := ZnUrl new addPathSegment: 'bar.txt'.
	self assert: (url inContextOf: baseUrl) equals: 'http://zn.stfx.eu/bar.txt' asZnUrl.
	"The following holds for all URLs"
	self assert: (baseUrl asRelativeUrl inContextOf: baseUrl) equals: baseUrl