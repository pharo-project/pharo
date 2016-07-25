protocol
openOnNil
	<script: '(DynamicSpecExample object: nil) open'>
	| ui layout |	
	ui := DynamicComposableModel new.
	ui title: self title.
	ui instantiateModels: #( text TextInputFieldModel ).
	ui text 
		text: 'Object is nil';
		enabled: false.
	
	layout := SpecLayout composed
				newRow: #text height: 25;
				yourself.
				
	ui openWithSpecLayout: layout.