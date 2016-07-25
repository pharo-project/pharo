private
load: aCollection
	errors removeAll.
	aCollection do: [:file | file fileIn ].