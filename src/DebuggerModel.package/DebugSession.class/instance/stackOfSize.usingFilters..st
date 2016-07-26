accessing
stackOfSize: limit usingFilters: stackFilters
	"Answer an OrderedCollection of the top 'limit' contexts on the receiver's sender chain, according to the given filters.
	
	Consider the following stack:   
		a <-- top of the stack
		b
		a
		c
		a
		b
		d
		e
		a <-- bottom of the stack
		
	If the given filters do not match context 'a' and 'b' the following stack will be returned:
		a <-- top of the stack
		b
		a
		c
		d
		e <-- bottom of the stack
		
	First all consecutive contexts starting from the top of the stack for which a filter does not match are 
	added to the result (a b a in the example). Then from the remaining stack only the context for which all
	filters match are added to the  result.
	"

	| stack context |
	stack := OrderedCollection new.
	context := interruptedContext.
	
	"Starting from the top of the stack add all consecutive contexts that should not be displayed."
	[ context ~~ nil
	 	and: [(stack size < limit)
			and: [ (self shouldDisplayContext: context basedOnFilters:  stackFilters) not ] ] ]  whileTrue: [
				stack addLast: context.
				context := context sender ].

	"Add all contexts that should be displayed. (the current context is checked again)"
	[context ~~ nil
		and: [stack size < limit ] ] whileTrue: [
			(self shouldDisplayContext: context basedOnFilters: stackFilters) 
				ifTrue: [ stack addLast: context ].
			context := context sender ].
	^ stack