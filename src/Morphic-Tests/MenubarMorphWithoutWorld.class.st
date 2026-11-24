"
I am the menubar as it is while Pharo loads, before Morphic's initialization installs a world: my current world is nil.
"
Class {
	#name : 'MenubarMorphWithoutWorld',
	#superclass : 'MenubarMorph',
	#category : 'Morphic-Tests-Support',
	#package : 'Morphic-Tests',
	#tag : 'Support'
}

{ #category : 'accessing' }
MenubarMorphWithoutWorld class >> currentWorld [
	"No world is installed yet"

	^ nil
]
