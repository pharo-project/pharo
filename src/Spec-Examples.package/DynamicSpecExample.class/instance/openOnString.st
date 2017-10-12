protocol
openOnString
	<script: '(DynamicSpecExample object: ''Pharo'') open'>
	
	| anUI layout bottomLayout temp |	
	anUI := DynamicComposablePresenter new.
	anUI title: self title.
	anUI instantiatePresenters: #( 
		label LabelPresenter
		check CheckBoxPresenter
		text TextInputFieldPresenter
		button ButtonPresenter ).
		
	anUI label label: object value.

			
	object whenChangedDo: [ :o | anUI label label: o asString ].
		
	anUI text 
		text: object value;
		enabled: true;
		acceptBlock: [ :string | self object: string ].
	
	temp := object value.
	
	anUI button
		label: 'reset';
		state: false;
		action: [ 
			anUI text text: temp.
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
	
	anUI check
		label: 'Label on top';
		state: true;
		whenActivatedDo: [ 
			anUI needFullRebuild: false.
			anUI buildWithSpecLayout: layout ];
		whenDeactivatedDo: [ 
			anUI needFullRebuild: false.
			anUI buildWithSpecLayout: bottomLayout ].
				
	anUI openWithSpecLayout: layout.