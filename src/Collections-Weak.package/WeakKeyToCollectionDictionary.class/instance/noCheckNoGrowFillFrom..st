private
noCheckNoGrowFillFrom: anArray
	"Add the elements of anArray except nils and associations with empty collections (or with only nils) to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	tally := 0.
	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :association |
			association key ifNotNil: [ :key | "Don't let the key go away"
				| cleanedValue |
				(cleanedValue := association value copyWithout: nil) isEmpty 
					ifFalse: [
						association value: cleanedValue.
						array
							at: (self scanForEmptySlotFor: key)
							put: association.
						tally := tally + 1 ] ] ] ]