private
writeStuffOnThenClose: aPipe

	^ [(1 to: 10) do:
		[:i |
		[aPipe nextPutAll: 'this is line ', i printString; cr]
			on: Error
			do: ["Ignore error. Test case will still fail, and throwing an error in this
				process would cause a debugger to be scheduled inconveniently."].
		(Delay forMilliseconds: 50) wait].
	(aPipe respondsTo: #writer)
		ifTrue: [[aPipe writer close]
					on: Error
					do: ["Ignore error to avoid a debugger"]]
		ifFalse: [[aPipe close]
					on: Error
					do: ["Ignore error to avoid a debugger"]]]
		forkAt: Processor userBackgroundPriority