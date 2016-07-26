accessing
nextOrNilSuchThat: aBlock
	"Fetch an object from queue that satisfies aBlock, skipping (but not removing) any intermediate objects.
	If no object has been found, answer <nil> and leave me intact.
	
	NOTA BENE:  aBlock can contain a non-local return (^).
	Found item is removed from queue	.
	
	If queue currently in the middle of extraction by other process, don't wait and return <nil> immediately"
	
	| item first |

	(item := dummy makeCircular) == dummy  ifTrue: [ ^ nil ]. 
	first := item.
	
	[ | object |
		object := item object.
		object == dummy ifFalse: [
			( [ aBlock value: object ] ifCurtailed: [ dummy next: first ] ) ifTrue: [
				item object: dummy.
				dummy next: first.
				^ object
			] ].
	
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: first.
			^ nil
			].
		item := item next.
	] repeat.
	