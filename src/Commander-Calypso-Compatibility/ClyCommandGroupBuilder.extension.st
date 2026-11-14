Extension { #name : 'ClyCommandGroupBuilder' }

{ #category : '*Commander-Calypso-Compatibility' }
ClyCommandGroupBuilder >> buildMenuFor: strategyClass context: aToolContext pureRoots: rootClasses [
	"Answers the Commander2 tree of the menu of aToolContext from the activations of strategyClass visible in it, then the pure commands of rootClasses."

	| activations |
	activations := OrderedCollection new.
	strategyClass visibleInstancesInContext: aToolContext do: [ :activation | activations add: activation ].
	^ self buildMenuOf: activations context: aToolContext pureRoots: rootClasses
]

{ #category : '*Commander-Calypso-Compatibility' }
ClyCommandGroupBuilder >> buildToolbarFor: strategyClass contexts: toolContexts [
	"Answers the Commander2 tree of a toolbar or of the browser tab actions from the activations of strategyClass visible in each context."

	^ self
		  buildToolbarOf: [ :toolContext |
				  | activations |
				  activations := OrderedCollection new.
				  strategyClass visibleInstancesInContext: toolContext do: [ :activation | activations add: activation ].
				  activations ]
		  contexts: toolContexts
]
