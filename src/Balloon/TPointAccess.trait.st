"
Contains methods for accessing points stored in variable word classes.
Initially used by PointArray & ShortPointArray
"
Trait {
	#name : #TPointAccess,
	#category : #Balloon-Collections
}

{ #category : #accessing }
TPointAccess >> at: index [

	^(super at: index * 2 - 1) @ (super at: index * 2)
]

{ #category : #accessing }
TPointAccess >> at: index put: aPoint [

	super at: index * 2 - 1 put: aPoint x asInteger.
	super at: index * 2 put: aPoint y asInteger.
	^aPoint
]

{ #category : #accessing }
TPointAccess >> bounds [
	| min max |
	min := max := self at: 1.
	self do:[:pt|
		min := min min: pt.
		max := max max: pt].
	^min corner: max
		
]

{ #category : #accessing }
TPointAccess >> defaultElement [ 

	"Return the default element of the receiver"
	^0@0
]
