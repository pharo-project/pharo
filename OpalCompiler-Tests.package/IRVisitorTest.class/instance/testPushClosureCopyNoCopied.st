testing
testPushClosureCopyNoCopied
	| ir |
	ir := IRBuilderTest new testPushClosureCopyNoCopied.
	self interpret: ir