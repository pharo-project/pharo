private-exceptions
unwindTo: aContext

	| context unwindBlock |
	context := self.
	[ (context := context findNextUnwindContextUpTo: aContext) isNil ] whileFalse: [
		(context unwindComplete) ifNil: [
			context unwindComplete: true.
			unwindBlock := context unwindBlock.
			unwindBlock value ]].
