"
Tracks how often completion entries are used
"
Class {
	#name : 'CoAdHocUsageTracker',
	#superclass : 'Object',
	#classVars : [
		'UsageCounts'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'tracking' }
CoAdHocUsageTracker class >> incrementUsageFor: aString [
	UsageCounts ifNil: [ UsageCounts := Dictionary new ].
	UsageCounts at: aString ifAbsentPut: [ 0 ].
	UsageCounts at: aString put: (UsageCounts at: aString) + 1.
	"The following expression acts as a flattener over the previous choices. 
	This way the user is not forced to leave with selection made in the last century."
	UsageCounts keysAndValuesDo: [ :k :v | k = aString 
			ifFalse: [ UsageCounts at: k put: (UsageCounts at: k) * 0.5 ]]
]

{ #category : 'class initialization' }
CoAdHocUsageTracker class >> reset [
     <script>
     UsageCounts := nil
]

{ #category : 'tracking' }
CoAdHocUsageTracker class >> usageCountFor: aString [
	UsageCounts ifNil: [ ^ 0 ].
	^ UsageCounts at: aString ifAbsent: [ 0 ].

]
