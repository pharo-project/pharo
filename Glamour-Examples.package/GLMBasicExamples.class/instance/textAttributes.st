composite
textAttributes
	"self new textAttributes openOn: 1"
	<glmBrowser: 'Text attributes' input: '1'>	
	| browser |
	browser := GLMTabulator new.
	browser row: #list.
	browser transmit to: #list; andShow: [ :a |
		a text
			format: [ :x | ('Title' asText) 
				makeColor: Color blue from: 2 to: 4; 
				makeColor: Color red from: 4 to: 5; "notice overlap"
				addAttribute: (TextFontReference toFont: (LogicalFont familyName: 'Open Sans' pointSize: 36)) from: 1 to: 1; "ignored when font family cannot be found"
				yourself.]].
	^ browser 