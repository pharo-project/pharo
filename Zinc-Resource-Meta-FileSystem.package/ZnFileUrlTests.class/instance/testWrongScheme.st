testing
testWrongScheme
	self should: [ 'http://zn.stfx.eu/zn/small.html' asZnUrl asFileReference ] raise: AssertionFailure