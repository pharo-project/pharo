Extension { #name : 'BlockClosure' }

{ #category : '*Reflectivity' }
BlockClosure >> rfEnsure: aBlock [
	"same as #esure, carefully written to never have active meta-links as it is called in the code path that checks for recursion"

	<primitive: 198>
	<metaLinkOptions: #( + optionDisabledLink)>
	| handler complete returnValue |
	"The handler should be in the first temporary to find it easily and support more complex unwind methods"
	handler := aBlock.
	returnValue := self rfvalueNoContextSwitch.
	complete ifNil:[
		complete := true.
		aBlock rfvalue.
	].
	^ returnValue
]

{ #category : '*Reflectivity' }
BlockClosure >> rfvalue [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 207>
	<metaLinkOptions: #( + optionDisabledLink)>
	| newContext |
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : '*Reflectivity' }
BlockClosure >> rfvalueNoContextSwitch [
	"same as valueNoContextSwitch, for recursion stopping metalinks"
	<primitive: 209>
	<metaLinkOptions: #( + optionDisabledLink)>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed
]

{ #category : '*Reflectivity' }
BlockClosure >> valueSelector [
	self numArgs = 0 ifTrue: [^#value].
	^(String streamContents: [:stream |
		stream nextPutAll: 'value:'.
		(self numArgs - 1) timesRepeat: [stream nextPutAll: 'value:']]) asSymbol
]
