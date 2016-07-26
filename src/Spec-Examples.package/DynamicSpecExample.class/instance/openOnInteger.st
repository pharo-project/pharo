protocol
openOnInteger
	<script: '(DynamicSpecExample object: 42) open'>
	
	| ui layout |
	
	ui := DynamicComposableModel new.
	ui title: self title.
	ui instantiateModels: #( 
		text LabelModel
		plus ButtonModel
		minus ButtonModel ).
		
	ui text label: object value asString.
	ui minus 
		label: '-';
		state: false;
		action: 	[ 
			object value: object value -1.
			ui text label: object value asString ].

	ui plus 
		label: '+';
		state: false;
		action: 	[ 
			object value: object value +1.
			ui text label: object value asString ].
	
	layout := SpecLayout composed
		newColumn: [ :c |
			c
				add: #text height: 25;
				newRow: [ :r | r add: #minus ; addSplitter; add: #plus ] height: 25 ];
		yourself.
				
	ui openWithSpecLayout: layout.