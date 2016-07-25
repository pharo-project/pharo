handling
transform
	| context node rewriteRule aMethod|
	self class environment 
		at:  #RBParseTreeRewriter 
		ifAbsent: [ ^	self signal ].
	context := thisContext sender sender sender.
	aMethod := context method.
	node := context sourceNodeExecuted.
	rewriteRule := (self class environment at: #RBParseTreeRewriter) new 
		replace: rule key with: rule value.
	(rewriteRule executeTree: node)
		ifFalse: [ ^ self ].
	node replaceWith: rewriteRule tree. 
	Author 
		useAuthor: 'AutoDeprecationRefactoring'
		during: [aMethod origin compile: aMethod ast formattedCode classified: aMethod protocol].	
	Log 
		ifNotNil: [:log | log add: self].
	self logTranscript