"
I load the UIManager package: the `UIManager` class, through which code asks the user questions without knowing which user interface runs.

I am my own baseline because both the debuggers (`BaselineOfDebugging`) and Morphic (`BaselineOfMorphicCore`) use the UI manager, and Morphic must not depend on the debuggers.
"
Class {
	#name : 'BaselineOfUIManager',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfUIManager',
	#package : 'BaselineOfUIManager'
}

{ #category : 'baseline' }
BaselineOfUIManager >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec package: 'UIManager'.
		"UIManager has no tests yet: its ISO stage only checks that it loads on its own"
		spec
			group: 'Core' with: #( 'UIManager' );
			group: 'Tests' with: #( 'Core' ) ]
]
