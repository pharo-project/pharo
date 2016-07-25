*opalcompiler-core
transformLastToReturn
	| last |
	statements ifEmpty: [ ^self].
	last := statements last.
	last isReturn ifFalse: [self replaceNode: last withNode: (RBReturnNode return: last start value: last )]