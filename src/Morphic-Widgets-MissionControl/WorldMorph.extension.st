Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Widgets-MissionControl' }
WorldMorph >> dismissMissionControl [


	| current |

	"Dismiss Mission Control if active"
	current := self submorphs detect: [ :m | m isKindOf: MissionControlMorph ] ifNone: [ nil ].
	current ifNotNil: [ current deactivate ]
]

{ #category : '*Morphic-Widgets-MissionControl' }
WorldMorph >> missionControl [

	"Toggle Mission Control"

	MissionControlMorph ensureMissionControlInWorld: self
]

{ #category : '*Morphic-Widgets-MissionControl' }
WorldMorph >> missionControlWithSameType [

	"Toggle Mission Control with the same type of tabs"

	MissionControlMorph ensureMissionControlInWorldWithSameTabs: self
]
