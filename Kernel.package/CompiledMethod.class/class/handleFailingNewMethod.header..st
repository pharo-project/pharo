private
handleFailingNewMethod: numberOfBytes header: headerWord
	"This newMethod:header: gets sent after newMethod:header: has failed
	 and allowed a scavenging garbage collection to occur.  The scavenging
	 collection will have happened as the VM is activating the (failing) basicNew:.
	 If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, headerWord, and the number of bytecodes specified
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough
	 memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79>
	| bytesRequested |
	bytesRequested := (headerWord bitAnd: 16rFFFF) + 1 * Smalltalk wordSize + numberOfBytes + 16.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingNewMethod: numberOfBytes header: headerWord