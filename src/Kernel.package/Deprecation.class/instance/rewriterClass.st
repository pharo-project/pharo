private
rewriterClass
	^ self class environment at: #RBParseTreeRewriter ifAbsent: [ nil ]