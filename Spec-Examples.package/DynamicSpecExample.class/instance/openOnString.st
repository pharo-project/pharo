protocol
openOnString
	<script: '(DynamicSpecExample object: ''Pharo'') open'>
	
	| ui layout bottomLayout temp |	
	ui := DynamicComposableModel new.
	ui title: self title.
	ui instantiateModels: #( 
		label LabelModel
		check CheckBoxModel
		text TextInputFieldModel
		button ButtonModel ).
		
	ui label label: object value.

			
	object whenChangedDo: [ :o | ui label label: o asString ].
		
	ui text 
		text: object value;
		enabled: true;
		acceptBlock: [ :string | self object: string ].
	
	temp := object value.
	
	ui button
		label: 'reset';
		state: false;
		action: [ 
			ui text text: temp.
			self object: temp ].
		
	layout := SpecLayout composed
				newColumn: [ :c |
					c 
						newRow: [ :r | r add: #label; add: #check width: 100 ] height: 25;
						newRow: [ :r | r add: #text; add: #button ] height: 25 ];
				yourself.
				
	bottomLayout := SpecLayout composed
				newColumn: [ :c |
					c 
						newRow: [ :r | r add: #text; add: #button ] height: 25;
						newRow: [ :r | r add: #label; add: #check width: 100 ] height: 25 ];
				yourself.
	
	ui check
		label: 'Label on top';
		state: true;
		whenActivatedDo: [ 
			ui needFullRebuild: false.
			ui buildWithSpecLayout: layout ];
		whenDeactivatedDo: [ 
			ui needFullRebuild: false.
			ui buildWithSpecLayout: bottomLayout ].
				
	ui openWithSpecLayout: layout.