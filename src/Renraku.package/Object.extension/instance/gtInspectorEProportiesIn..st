*renraku
gtInspectorEProportiesIn: composite
	"This provides a list of all external properties"

	<gtInspectorPresentationOrder: 20>
	| eProperties |
	composite list
		when:
				[ QASettings inspectorPluggin and: [ (eProperties := self externalProperties) notEmpty ] ];
		title: [ eProperties size asString , (' property' asPluralBasedOn: eProperties) ];
		titleIcon: [ CircleMorph new
				extent: 16 @ 16;
				borderWidth: 0;
				color: Color red darker darker;
				addMorphCentered:
					(eProperties size asStringMorph
						color: Color white;
						emphasis: 1;
						yourself);
				iconOrThumbnail ];
		display: [ self externalProperties ];
		icon: #icon;
		format: #title