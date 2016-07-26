*Komitter-UI
label
	| url |
	url := self remote location splitOn: '/'.
	
	^ String streamContents: [ :stream | stream << (url at: 5) ]