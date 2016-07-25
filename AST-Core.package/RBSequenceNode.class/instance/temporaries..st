accessing
temporaries: tempCollection 
	temporaries := tempCollection.
	temporaries do: [:each | each parent: self]