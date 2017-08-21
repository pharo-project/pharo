sorting
sorted
	"Return a new sequenceable collection which contains the same elements as self but its 
elements are sorted in ascending order using the #'<=' operator."
	
	^self sorted: [ :a :b| a <= b ]