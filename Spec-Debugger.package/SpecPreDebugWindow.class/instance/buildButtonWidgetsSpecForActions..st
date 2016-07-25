building widgets
buildButtonWidgetsSpecForActions: aCollection

	^ aCollection
		inject: OrderedCollection new
		into: [ :specs :action |
			specs 
				add: action specId;
			 	add: action specModel;
				yourself ]
	