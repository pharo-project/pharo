Extension { #name : #SearchableList }

{ #category : #'*Spec-Examples' }
SearchableList class >> example [
	<sampleInstance>
	"self example"

	^ self new
		items: #(#aaa #abc #axz #bbb #bfbg #bof #c); openWithSpec
]
