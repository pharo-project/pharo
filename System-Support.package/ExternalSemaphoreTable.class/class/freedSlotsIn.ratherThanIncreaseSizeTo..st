private
freedSlotsIn: externalObjects ratherThanIncreaseSizeTo: newSize
	"In some VM's, the external object table has a max size, which has to be increased for vm to reference them correctly."
	"In that case, try to gc to free slots first before actually increasing the max size"
	"Return whether I ended up freeing slots by GC'ing, or one should increase the size of "
	^Smalltalk vm maxExternalSemaphores
		ifNotNil: [:maxSize |
			(maxSize < newSize) and: 
				[| needToGrow |
				Smalltalk garbageCollect.
				"Do we have free slots now? If not, performing the GC didn't help and we still have to grow."
				needToGrow := externalObjects includes: nil.
				
				needToGrow 
					ifTrue: ["If we did GC, warn we had to gc so actions could be taken if appropriate."
						self traceCr: DateAndTime now printString;
							traceCr:  'WARNING:  Had to GC to make room for more external objects.';
							traceCr: 'If this happens often, it would be a good idea to either:';
							traceCr: '- Raise the maxExternalSemaphores size.';
							traceCr: '- Write your code to explicitly release them rather than wait for finalization.']
					ifFalse:  [Smalltalk vm maxExternalSemaphores: newSize].
				needToGrow]]
		ifNil:[false]