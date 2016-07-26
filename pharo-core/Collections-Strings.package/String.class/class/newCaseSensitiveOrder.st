initialization
newCaseSensitiveOrder
	"Case-sensitive compare sorts space, digits, letters, all the rest..."
	
	| newTab order |
	newTab := ByteArray new: 256 withAll: 255.
	order := -1.
	' 0123456789' do:  "0..10"
		[:c | newTab at: c asciiValue + 1 put: (order := order+1)].
	($a to: $z) do:     "11-64"
		[:c | newTab  at: c asUppercase asciiValue + 1 put: (order := order+1).
		newTab  at: c asciiValue + 1 put: (order := order+1)].
	1 to: newTab  size do:
		[:i | (newTab  at: i) = 255 ifTrue:
			[newTab  at: i put: (order := order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	^ newTab 