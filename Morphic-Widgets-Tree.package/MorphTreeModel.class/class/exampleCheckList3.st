examples
exampleCheckList3
	"self exampleCheckList3"
	Cursor wait 
		showWhile: [
			^ ((self new
				beCheckList;
				autoMultiSelection: true;
				rootItems: (Object allSubclasses sort: [:a :b | a name <= b name]);
				 headerLabel: 'Sub-classes';
				defaultTreeMorph) 
					buildContents;
					embeddedInMorphicWindowLabeled: 'All Object sub-classes ready to be checked') 
						openInWorld]