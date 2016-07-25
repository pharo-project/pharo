private
sort: aCollection
	^ aCollection asOrderedCollection sort: [:e1 :e2 | (e1 asString) < (e2 asString)].