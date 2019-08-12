Extension { #name : #Collection }

{ #category : #'*Calypso-Browser' }
Collection >> nonIntersect: aCollection [
	"Answer the set of left join between two collections excluding intersection values."

	"(#(1 2 3 4) nonIntersect: #(3 4 5)) >>> #(1 2)"

	"(#(1 2 3 4) nonIntersect: #()) >>> #(1 2 3 4)"

	"(#() nonIntersect: #(1 2 3 4)) >>> #()"

	^ self copy reject: [ :e | (self intersection: aCollection) includes: e ]
]
