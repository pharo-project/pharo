compiling
compiling: sourceCode shouldRaise: exceptionClass

	self should: [ self compile: sourceCode ] raise: exceptionClass