private
push: numObjects fromIndexable: anIndexableCollection
	"Push the elements of anIndexableCollection onto the receiver's stack.
	 Do not call directly.  Called indirectly by {1. 2. 3} constructs."

	1 to: numObjects do: [ :i |
		self push: (anIndexableCollection at: i)]