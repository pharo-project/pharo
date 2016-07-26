accessing
arguments: anArray
	arguments := WeakArray withAll: anArray.
	"no reason this should be a WeakArray"
	shouldBeNil := Array withAll: (anArray collect: [ :ea | ea isNil ]).
