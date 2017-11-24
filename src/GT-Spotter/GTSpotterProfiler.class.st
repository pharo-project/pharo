Class {
	#name : #GTSpotterProfiler,
	#superclass : #KMCategory,
	#category : #'GT-Spotter-Utils'
}

{ #category : #testing }
GTSpotterProfiler class >> isGlobalCategory [
	^ true
]

{ #category : #profiling }
GTSpotterProfiler class >> profileOneCycle [
	"Tally only the UI process"
	
	(self confirm: 'MessageTally the UI process one cycle')
		ifTrue: [TimeProfiler
			onBlock: [
				|spotterModel done|
				done := false.
				GTSpotterGlobalShortcut reset.
				spotterModel := GTSpotter new.
				
				spotterModel announcer when: GTSpotterSearchFinished do: [ done := true ].
					
				GTSpotterMorph new
					extent: (World width / 2.4 @ (World height / 1.6)) asIntegerPoint;
					spotterModel: spotterModel;
					openCenteredInWorld.
		
				spotterModel class markOpened.
				[ done ] whileFalse: [ World doOneCycle ] ] ]
]

{ #category : #shortcuts }
GTSpotterProfiler >> keymapProfileOneCycle [
	"KMRepository reset"
	<shortcut>
	^ KMKeymap
			shortcut: $u meta
			action: [ self class profileOneCycle ]
]
