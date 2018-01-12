"
I'm a Demo that show the usage of Metalink. When I'm installed in Nautilus I glow red whenever the selected method is being executed anywhere in the system 
"
Class {
	#name : #ExecutionDisplayPlugin,
	#superclass : #AbstractNautilusPlugin,
	#instVars : [
		'morph',
		'link'
	],
	#category : #'Nautilus-Plugins-Examples'
}

{ #category : #position }
ExecutionDisplayPlugin class >> defaultPosition [
		
	^ #middle
]

{ #category : #information }
ExecutionDisplayPlugin class >> description [
	^ 'A demo of Metalink usage: when it is installed, it glows red when the selected method is executed somewhere in the system'
]

{ #category : #display }
ExecutionDisplayPlugin class >> pluginName [

	^'Execution Metalink Demo'
]

{ #category : #information }
ExecutionDisplayPlugin class >> possiblePositions [
	"Return all the possible positions for a plugin. Subclasses are not intented to override this method."
	^ {	#middle.
		#none. }
]

{ #category : #display }
ExecutionDisplayPlugin >> display [

	^morph.
]

{ #category : #initialization }
ExecutionDisplayPlugin >> initialize [

	super initialize.
	morph := ExecutionDisplayMorph new height: 20.
	link := MetaLink new metaObject: morph; selector: #noteExecuted.
]

{ #category : #private }
ExecutionDisplayPlugin >> isMethodAlreadyLinked: aMethod [
	^ aMethod notNil
		and: [ link methods anySatisfy: [ :m | m selector = aMethod selector and: [ m methodClass = aMethod methodClass ] ] ]
]

{ #category : #'execution Visualizer' }
ExecutionDisplayPlugin >> noteExectuted: anAnnouncement [
	(self isMethodAlreadyLinked: anAnnouncement method)
		ifTrue: [ ^ self ].
	link uninstall.
	anAnnouncement method ifNotNil: [ :method | method ast link: link ]
]

{ #category : #registration }
ExecutionDisplayPlugin >> registerTo: aModel [

	aModel announcer
   	 when: NautilusMethodSelected 
	 send: #noteExectuted:
	 to: self
]
