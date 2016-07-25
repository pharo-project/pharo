transforming
transform
	target compile: (class sourceCodeFor: selector) classified: (class protocolsFor: selector).
	class removeMethod: selector