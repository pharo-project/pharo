handling
transform
	| node rewriteRule aMethod|
	self rewriterClass ifNil:[ ^ self signal ].
	aMethod := self contextOfSender method.
	"no need to transform doits"
	aMethod isDoIt ifTrue:[^ self].
	node := self contextOfSender sourceNodeExecuted.
	rewriteRule := self rewriterClass new 
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