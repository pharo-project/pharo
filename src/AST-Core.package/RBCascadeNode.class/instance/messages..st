accessing
messages: messageNodeCollection 
	messages := messageNodeCollection.
	messages do: [:each | each parent: self]