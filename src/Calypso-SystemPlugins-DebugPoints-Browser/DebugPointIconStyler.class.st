"
I style the the code window of the calypso browser to show debug points.
"
Class {
	#name : 'DebugPointIconStyler',
	#superclass : 'IconStyler',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
DebugPointIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'styling' }
DebugPointIconStyler >> addIconStyle: aNode from: start to: stop color: aColor [
	"there probably is a better way that just adding a second row but i couldn't figure out rubric"
	| conf |
	(self shouldStyleNode: aNode) ifFalse: [ ^ self ].

	"list options for each debug point"
	(self allDebugPointsOn: aNode) do: [ :dp |
		
		conf := RubConfigurationChange new.
	conf configurationBlock: [ :text | 
		| r r2 |
		r := self segmentMorphClass from: start to: stop + 1.
		text addSegment: r.
		r label: (self iconLabelBlock: dp).
		r icon: (self iconFor: dp).
		r iconBlock: (self iconBlock: dp).
		r color: aColor.
		r borderColor: self borderColor .
		r2 := self segmentMorphClass from: start to: stop + 1.
		text addSegment: r2.
		r2 label: 'Remove'.
		r2 icon: (self iconFor: dp ).
		r2 iconBlock: (self iconBlockEdit: dp).
		r2 color: aColor.
		r2 borderColor: self borderColor .
		].
	
	textModel announce: conf 
	].
	
	
	
	"conf := RubConfigurationChange new.
	conf configurationBlock: [ :text | 
		| r r2 |
		r := self segmentMorphClass from: start to: stop + 1.
		text addSegment: r.
		r label: (self iconLabelBlock: aNode).
		r icon: (self iconFor: aNode).
		r iconBlock: (self iconBlock: aNode).
		r color: aColor.
		r borderColor: self borderColor .
		r2 := self segmentMorphClass from: start to: stop + 1.
		text addSegment: r2.
		r2 label: 'Remove'.
		r2 icon: (self iconFor: aNode).
		r2 iconBlock: (self iconBlockEdit: aNode).
		r2 color: aColor.
		r2 borderColor: self borderColor .
		].
	
	textModel announce: conf"
]

{ #category : 'private' }
DebugPointIconStyler >> allDebugPointsOn: aNode [

	^(aNode links select: [ :link | link metaObject isKindOf: self forClass ]) collect: [:link | link metaObject].
]

{ #category : 'private' }
DebugPointIconStyler >> debugPointOn: aNode [
	^(aNode links detect: [ :link | link metaObject isKindOf: self forClass ]) metaObject.
]

{ #category : 'styling' }
DebugPointIconStyler >> disableIcon: anIcon dp: dp [

	dp enabled ifFalse: [
		^anIcon dimmed
		"in case i want it crossed out when disabled:"
		"^anIcon mergeWith: (self iconNamed: #smallCancel)"
		].
	
	^anIcon
]

{ #category : 'private' }
DebugPointIconStyler >> forClass [
	^DebugPoint
]

{ #category : 'testing' }
DebugPointIconStyler >> hasBehaviorIcon: anIcon dp: dp [

	dp behaviors ifNotEmpty: [
		^ anIcon
			  mergeWith:
				  (((self iconNamed: #smallCogInitialState) scaledToSize: 9 @ 9)
					   darker: 1)
			  at: 8 @ 0 ].
	^ anIcon
]

{ #category : 'defaults' }
DebugPointIconStyler >> highlightColor [

	^ Color yellow alpha: 0.3
]

{ #category : 'accessing' }
DebugPointIconStyler >> icon: dp [
	
	^ dp properties at: #Icon ifAbsent: [ #smallDebug ].
]

{ #category : 'defaults' }
DebugPointIconStyler >> iconBlockEdit: dp [
		 ^ [ :seg | dp remove.
	   seg delete ]
]

{ #category : 'defaults' }
DebugPointIconStyler >> iconFor: dp [
	| icon |
	icon := (self iconNamed: (self icon: dp)) deepCopy.
	icon := self hasBehaviorIcon: icon dp: dp.
	icon := self disableIcon: icon dp: dp.

	^ icon
]

{ #category : 'defaults' }
DebugPointIconStyler >> iconLabel: dp [
	"| dp | 
	dp := (self debugPointOn: aNode)."
	(dp getBehavior: CountBehavior) ifNotNil: [ :bh | ^dp name, ' (Count: ', bh count asString,')' ].
	^ dp name
]

{ #category : 'testing' }
DebugPointIconStyler >> shouldStyleNode: aNode [

	^aNode hasDebugPointOfType: self forClass
]
