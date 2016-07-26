accessing
flushAllSuchThat: aBlock
	"Remove from the queue all objects that satisfy aBlock.
	Note, the operation is no-op, if queue is currently in the middle of extraction by other process"
	
	| item first |

	(item := dummy makeCircular) == dummy ifTrue: [ ^ self ]. 
	first := item.
	
	[ | object |
		object := item object.
		object == dummy ifFalse: [
			( [ aBlock value: object ] ifCurtailed: [ dummy next: first ] ) ifTrue: [
				item object: dummy.
			] ].
	
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: first.
			^ self
			].
		item := item next.
	] repeat.