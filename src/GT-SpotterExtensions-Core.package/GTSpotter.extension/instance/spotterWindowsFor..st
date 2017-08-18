*GT-SpotterExtensions-Core
spotterWindowsFor: aStep
	<spotterOrder: 5>

	aStep listProcessor
			title: 'Windows';
			candidatesLimit: 10;
			allCandidates: [ World submorphs 	select: [ :window | window isKindOf: SystemWindow ] ];
			itemName: [ :window | '{1} [{2}]' format: { window  label. window className } ];
			itemIcon: [ :window | window taskbarIcon ];
			sort:  [ :window1 :window2 | window1 label < window2 label ];
			filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true