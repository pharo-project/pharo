event-handling
refresh
	"refresh the object in the inspector, updating his modified instance variables"

	description hasUnacceptedEdits 
		ifTrue: [  ^ self ].
	description getSelection size = 0 
		ifTrue: [ self refreshDescription; updateList ]