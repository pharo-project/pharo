private
noCheckNoGrowFillFrom: anArray
	"Add the elements of anArray except nils and flag to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	tally := 0.
	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :association |
			association key 
				ifNil: [ finalizer ifNotNil: [ finalizer value: association value ] ]
				ifNotNil: [ :key | "Don't let the key go away"
					array
						at: (self scanForEmptySlotFor: key)
						put: association.
					tally := tally + 1 ] ] ]