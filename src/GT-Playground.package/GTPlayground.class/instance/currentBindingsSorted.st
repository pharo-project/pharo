accessing-dynamic
currentBindingsSorted

	^ self currentBindings associations asSortedCollection: [ :first :second | first key < second key ]