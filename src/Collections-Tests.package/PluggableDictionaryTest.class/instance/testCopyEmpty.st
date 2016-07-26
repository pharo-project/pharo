tests - copy - clone
testCopyEmpty
	| pluggableDict copiedPluggableDict |

	super testCopyEmpty.

	(pluggableDict := self emptyDict)
		equalBlock: [:a :b | a = b];
		hashBlock: [:a | a hash].

	copiedPluggableDict := pluggableDict copyEmpty.
	self assert: copiedPluggableDict equalBlock == pluggableDict equalBlock.
	self assert: copiedPluggableDict hashBlock == pluggableDict hashBlock.