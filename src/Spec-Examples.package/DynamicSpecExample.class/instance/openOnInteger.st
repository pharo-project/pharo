protocol
openOnInteger
	<script: '(DynamicSpecExample object: 42) open'>
	
	| anUI layout |
	
	anUI := DynamicComposablePresenter new.
	anUI title: self title.
	anUI instantiatePresenters: #( 
		text LabelPresenter
		plus ButtonPresenter
		minus ButtonPresenter ).
		
	anUI text label: object value asString.
	anUI minus 
		label: '-';
		state: false;
		action: 	[ 
			object value: object value -1.
			anUI text label: object value asString ].

	anUI plus 
		label: '+';
		state: false;
		action: 	[ 
			object value: object value +1.
			anUI text label: object value asString ].
	
	layout := SpecLayout composed
		newColumn: [ :c |
			c
				add: #text height: 25;
				newRow: [ :r | r add: #minus ; addSplitter; add: #plus ] height: 25 ];
		yourself.
				
	anUI openWithSpecLayout: layout.