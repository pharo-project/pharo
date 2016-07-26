private
copyForSaving
	"Answer a copy of the receiver suitable for serialization.
	 Notionally, if the receiver's outerContext has been returned from then nothing
	 needs to be done and we can use the receiver. But there's a race condition
	 determining if the receiver has been returned from (it could be executing in a
	 different process). So answer a copy anyway."
	^self shallowCopy postCopy