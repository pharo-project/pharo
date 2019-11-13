Extension { #name : #Object }

{ #category : #'*Graphics-Canvas' }
Object >> printDirectlyToDisplay [
	"For debugging: write the receiver's printString directly to the display at (0, 100); senders of this are detected by the check-for-slips mechanism."

	self asString displayAt: 0@100

"StringMorph someInstance printDirectlyToDisplay"
]
