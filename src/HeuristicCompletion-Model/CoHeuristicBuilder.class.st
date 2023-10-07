"
I am an heuristic builder.
I am composed with several small heuristics and generate a chain of responsibility from them in the right order.

For example, given a list of heuristics:

CoHeuristicBuilder new
	add: CoSelfMessageHeuristic new;
	add: CoSuperMessageHeuristic new;
	add: CoInitializeInferencedMessageHeuristic new;
	yourself.

I create the following chain:

CoSelfMessageHeuristic -next-> CoSuperMessageHeuristic -next-> CoInitializeInferencedMessageHeuristic
	
I am used also as a hook, that can be redefined in subclasses and build heuristics differently.
"
Class {
	#name : 'CoHeuristicBuilder',
	#superclass : 'Object',
	#instVars : [
		'heuristics'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'adding' }
CoHeuristicBuilder >> add: anHeuristic [

	heuristics add: anHeuristic
]

{ #category : 'building' }
CoHeuristicBuilder >> build [

	^ heuristics reduceRight: [ :left :right | left next: right. left ]
]

{ #category : 'initialization' }
CoHeuristicBuilder >> initialize [
	super initialize.
	heuristics := OrderedCollection new
]
