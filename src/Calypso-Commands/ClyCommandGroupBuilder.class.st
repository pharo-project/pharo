"
I build a Commander2 command group (CmCommandGroup) for Calypso tools.

I take a base command class (a subclass of CmCommand) and a Calypso tool context.
I instantiate every concrete subclass of the base class with that context and register
the resulting commands in a CmCommandGroup, preserving the order in which the class
hierarchy returns them.

Use ClyCommandGroupMenuRenderer to display the produced group inside a Morphic menu.
"
Class {
	#name : 'ClyCommandGroupBuilder',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandGroupBuilder class >> buildFor: aCommandClass context: aContext [

	^ self new
		  buildFor: aCommandClass
		  context: aContext
]

{ #category : 'building' }
ClyCommandGroupBuilder >> buildFor: aCommandClass context: aContext [

	| group |
	group := CmCommandGroup named: aCommandClass name asString.
	aCommandClass allSubclasses do: [ :aClass |
			aClass hasAbstractMethods ifFalse: [
					group register: (aClass new
							 context: aContext;
							 yourself) ] ].
	^ group
]
