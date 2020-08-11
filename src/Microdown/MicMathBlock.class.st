"
I'm a math environment and I have optional parameters

$
V_i = C_0 - C_3
$


"
Class {
	#name : #MicMathBlock,
	#superclass : #MicSameStartStopMarkupBlock,
	#instVars : [
		'arguments'
	],
	#category : #'Microdown-Model'
}

{ #category : #accessing }
MicMathBlock >> arguments [
	
	^ arguments ifNil: [ arguments := ZnResourceMetaUtils parseQueryFrom: firstLine readStream ]
]

{ #category : #markups }
MicMathBlock >> lineStartMarkup [

	^ parser mathOpeningBlockMarkup 
]
