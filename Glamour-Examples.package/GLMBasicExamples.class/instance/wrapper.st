browsers
wrapper 
	
	<glmBrowser: 'Wrapper' input: '1 to: 42'>
	"self new wrapper openOn: (1 to: 42)"
	
	| wrapper |
	wrapper := GLMWrapper new title: 'Wrapper'.
	wrapper show: [:a | a list title: 'List'. a text title: 'Text' ].
	^ wrapper