*renraku
gtInspectorEProportiesIn: composite
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