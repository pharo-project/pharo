tests
testIncludes
	"self debug: #testIncludes"
	
	| weakSet transientFakeNilObject |
	weakSet := WeakSet new.
	#(true nil 1) do: [ :each | self deny: (weakSet includes: each) ].
	weakSet add: true.
	self assert: (weakSet includes: true).
 	weakSet remove: true.
	self deny: (weakSet includes: true).
	transientFakeNilObject := ((1 to: 1000) detect: [ :each | each asString hash - nil hash \\ weakSet capacity = 0 ]) asString. "this string will occupy the same slot as nil would"

	weakSet add: transientFakeNilObject.
	transientFakeNilObject := transientFakeNilObject copy.
	Smalltalk garbageCollect. "get rid of transientFakeNilObject"
	self deny: (weakSet includes: transientFakeNilObject).
	self deny: (weakSet includes: nil)