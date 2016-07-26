private
sortByNameSize: collection

	^collection asSortedCollection: [:x :y | x name size < y name size]