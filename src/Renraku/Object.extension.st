Extension { #name : #Object }

{ #category : #'*renraku' }
Object >> externalProperties [

	| pragmas  externalProperties |
	pragmas := Pragma allNamed: #eProperty from: self class to: ProtoObject.
	externalProperties := OrderedCollection new.

	pragmas do: [ :pragma |
		[ externalProperties addAll:
			(pragma method
				valueWithReceiver:
				self arguments: #())
		]
			on: Exception
			do: [ :ex |
				ReExceptionStrategy current
					handle: ex
					about: self
					forProppertiesDo: [ :prop |
						externalProperties add: prop ]  ] ].
			
	^ externalProperties

]

{ #category : #'*renraku' }
Object >> glmCritiqueSourceIn: composite [

	"display nothing for now"
]

{ #category : #'*renraku' }
Object >> gtInspectorEProportiesIn: composite [
	"This provides a list of all external properties"

	<gtInspectorPresentationOrder: 20>
	| eProperties ePropLazy |
	
	ReSettings inspectorPluggin ifFalse: [ ^ self ].
	
	ePropLazy := [
		eProperties ifNil: [ eProperties := self externalProperties ] ].
	
	composite list
		when: [ ePropLazy value notEmpty ];
		title: [ ePropLazy value size asString, (' property' asPluralBasedOn: ePropLazy value) ];
		titleIcon: [ CircleMorph new
				extent: 16 @ 16;
				borderWidth: 0;
				color: Color red darker darker;
				addMorphCentered:
					(ePropLazy value size asStringMorph
						color: Color white;
						emphasis: 1;
						yourself);
				iconOrThumbnail ];
		display: [ ePropLazy value ];
		icon: #icon;
		format: #title
]
