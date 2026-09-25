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

{ #category : 'private' }
DebugPointIconStyler >> addIconStyle: aNode from: start to: stop color: aColor [

	(self shouldStyleNode: aNode) ifFalse: [ ^ self ].
	(self allDebugPointsOn: aNode) do: [ :dp |
			renderer addDecoration: (IconStylerDecoration new
					 name: self stylerName;
					 interval: (start to: stop);
					 highlightColor: aColor;
					 icon: (self iconFor: dp);
					 iconBlock: (self iconBlock: dp);
					 title: (self iconLabel: dp);
					 yourself).
			renderer addDecoration: (IconStylerDecoration new
					 name: self stylerName;
					 interval: (start to: stop);
					 highlightColor: aColor;
					 icon: (self iconFor: dp);
					 iconBlock: (self iconBlockEdit: dp);
					 title: 'Remove';
					 yourself) ]
]

{ #category : 'private' }
DebugPointIconStyler >> allDebugPointsOn: aNode [

	^(aNode links select: [ :link | link metaObject isKindOf: self forClass ]) collect: [:link | link metaObject].
]

{ #category : 'building' }
DebugPointIconStyler >> buildIconStyleFor: dp to: aNode [

	^ IconStylerDecoration new
		  name: self stylerName;
		  interval: (aNode start to: aNode stop + 1);
		  highlightColor: self highlightColor;
		  icon: (self iconFor: dp);
		  iconBlock: (self iconBlock: dp);
		  title: (self iconLabel: dp);
		  yourself
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

	^ [ :seg |
		  | interval |
		  interval := seg interval.
		  dp remove.
		  renderer textModel announce: (RubConfigurationChange new configurationBlock: [ :scrolledTextMorph |
				   (scrolledTextMorph textArea segments select: [ :s | s interval = interval ]) do: [ :s | s delete ] ]) ]
]

{ #category : 'defaults' }
DebugPointIconStyler >> iconFor: dp [
	| icon |
	icon := (self iconNamed: (self icon: dp)) copy.
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

{ #category : 'private' }
DebugPointIconStyler >> stylerName [

	^ 'debug-point'
]
