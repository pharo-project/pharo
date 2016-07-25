examples
exampleCheckList1
	"self exampleCheckList1"
	Cursor wait 
		showWhile: [
			^ self new
				beCheckList;
				rootItems: (Object allSubclasses sort: [:a :b | a name <= b name]);
				 headerLabel: 'Sub-classes';
				 openDialogWindowIn: World title: 'All Object sub-classes ready to be checked']