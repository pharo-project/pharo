text
pharoScript
	<glmBrowser: 'Pharo script' input: '''"Enter some Smalltalk code and then accept to preview the result."'''>
	"self new pharoScript openOn: ''"
	| browser |
	browser := GLMTabulator new.
	browser row: #code span: 2; row: #preview.
	browser transmit to: #code; andShow: [ :a | 
		a pharoScript  
			title: 'Smalltalk Code';
			populate: #acceptedCode icon: GLMUIThemeExtraIcons glamorousAccept on: $s entitled: 'Accept' with: [:text | text text ] ].
	browser transmit from: #code port: #acceptedCode; to: #preview; andShow: [ :a | 
		a text 
			title: 'Evaluated result';
			display: [:code | [Smalltalk evaluate: code ] on: Error do: ['Error']];
			act: [:text | text displayValue explore] icon: GLMUIThemeExtraIcons glamorousInspect entitled: 'Explore' ].
	^ browser